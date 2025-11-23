"""
Database Migration Manager
Tracks and applies database schema migrations
"""
import sqlite3
import logging
from pathlib import Path
from typing import List, Dict, Optional
from datetime import datetime


logger = logging.getLogger(__name__)


class MigrationManager:
    """Manages database schema migrations"""

    def __init__(self, db_path: str):
        """
        Initialize migration manager

        Args:
            db_path: Path to SQLite database file
        """
        self.db_path = Path(db_path)
        self.migrations_dir = Path(__file__).parent / 'migrations'
        self._ensure_migrations_table()

    def _get_connection(self) -> sqlite3.Connection:
        """Get database connection"""
        conn = sqlite3.connect(str(self.db_path))
        conn.row_factory = sqlite3.Row
        return conn

    def _ensure_migrations_table(self):
        """Create migrations tracking table if it doesn't exist"""
        with self._get_connection() as conn:
            conn.execute("""
                CREATE TABLE IF NOT EXISTS schema_migrations (
                    id INTEGER PRIMARY KEY AUTOINCREMENT,
                    migration_name TEXT UNIQUE NOT NULL,
                    applied_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                    checksum TEXT,
                    description TEXT
                )
            """)
            conn.commit()
            logger.info("Migrations tracking table ensured")

    def get_applied_migrations(self) -> List[str]:
        """
        Get list of already applied migration names

        Returns:
            List of migration names
        """
        with self._get_connection() as conn:
            cursor = conn.execute(
                "SELECT migration_name FROM schema_migrations ORDER BY id"
            )
            return [row['migration_name'] for row in cursor.fetchall()]

    def get_pending_migrations(self) -> List[Path]:
        """
        Get list of pending migrations that haven't been applied

        Returns:
            List of migration file paths
        """
        if not self.migrations_dir.exists():
            logger.warning(f"Migrations directory not found: {self.migrations_dir}")
            return []

        # Get all .sql files in migrations directory
        all_migrations = sorted(self.migrations_dir.glob('*.sql'))
        applied = set(self.get_applied_migrations())

        # Filter out already applied migrations
        pending = [m for m in all_migrations if m.stem not in applied]

        logger.info(f"Found {len(pending)} pending migrations out of {len(all_migrations)} total")
        return pending

    def apply_migration(self, migration_path: Path) -> bool:
        """
        Apply a single migration

        Args:
            migration_path: Path to migration SQL file

        Returns:
            bool: True if successful, False otherwise
        """
        migration_name = migration_path.stem

        try:
            logger.info(f"Applying migration: {migration_name}")

            # Read migration SQL
            with open(migration_path, 'r', encoding='utf-8') as f:
                sql = f.read()

            # Extract description from first comment line
            description = ""
            for line in sql.split('\n'):
                if line.strip().startswith('--'):
                    description = line.strip('- ').strip()
                    break

            # Apply migration
            with self._get_connection() as conn:
                # Execute migration SQL
                conn.executescript(sql)

                # Record migration as applied
                conn.execute("""
                    INSERT INTO schema_migrations (migration_name, description)
                    VALUES (?, ?)
                """, (migration_name, description))

                conn.commit()

            logger.info(f"Successfully applied migration: {migration_name}")
            return True

        except Exception as e:
            logger.error(f"Failed to apply migration {migration_name}: {e}")
            return False

    def apply_all_pending(self) -> Dict[str, any]:
        """
        Apply all pending migrations

        Returns:
            Dict with results: {
                'applied': int,
                'failed': int,
                'errors': List[str]
            }
        """
        pending = self.get_pending_migrations()
        results = {
            'applied': 0,
            'failed': 0,
            'errors': [],
            'migrations': []
        }

        if not pending:
            logger.info("No pending migrations to apply")
            return results

        for migration in pending:
            if self.apply_migration(migration):
                results['applied'] += 1
                results['migrations'].append(migration.stem)
            else:
                results['failed'] += 1
                results['errors'].append(f"Failed: {migration.stem}")

        logger.info(f"Migration summary: {results['applied']} applied, {results['failed']} failed")
        return results

    def rollback_migration(self, migration_name: str) -> bool:
        """
        Rollback a migration (if rollback SQL exists)

        Args:
            migration_name: Name of migration to rollback

        Returns:
            bool: True if successful
        """
        rollback_path = self.migrations_dir / f"{migration_name}_rollback.sql"

        if not rollback_path.exists():
            logger.error(f"No rollback script found for {migration_name}")
            return False

        try:
            logger.info(f"Rolling back migration: {migration_name}")

            with open(rollback_path, 'r', encoding='utf-8') as f:
                sql = f.read()

            with self._get_connection() as conn:
                conn.executescript(sql)
                conn.execute(
                    "DELETE FROM schema_migrations WHERE migration_name = ?",
                    (migration_name,)
                )
                conn.commit()

            logger.info(f"Successfully rolled back: {migration_name}")
            return True

        except Exception as e:
            logger.error(f"Failed to rollback {migration_name}: {e}")
            return False

    def get_migration_status(self) -> Dict[str, any]:
        """
        Get current migration status

        Returns:
            Dict with migration status information
        """
        applied = self.get_applied_migrations()
        pending = self.get_pending_migrations()

        return {
            'total_applied': len(applied),
            'total_pending': len(pending),
            'applied_migrations': applied,
            'pending_migrations': [m.stem for m in pending],
            'last_migration': applied[-1] if applied else None
        }

    def create_migration_file(self, name: str, description: str = "") -> Path:
        """
        Create a new migration file template

        Args:
            name: Name for the migration (will be prefixed with number)
            description: Optional description

        Returns:
            Path to created migration file
        """
        # Get next migration number
        existing = list(self.migrations_dir.glob('*.sql'))
        next_num = len(existing) + 1

        # Create filename
        filename = f"{next_num:03d}_{name}.sql"
        filepath = self.migrations_dir / filename

        # Create migration template
        template = f"""-- {description or name}
-- Created: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}

-- Add your migration SQL here
-- Example:
-- CREATE TABLE example (
--     id INTEGER PRIMARY KEY,
--     name TEXT NOT NULL
-- );

"""

        with open(filepath, 'w', encoding='utf-8') as f:
            f.write(template)

        logger.info(f"Created migration file: {filepath}")
        return filepath


def run_migrations(db_path: str) -> Dict[str, any]:
    """
    Convenience function to run all pending migrations

    Args:
        db_path: Path to database file

    Returns:
        Migration results
    """
    manager = MigrationManager(db_path)
    return manager.apply_all_pending()


if __name__ == "__main__":
    # Test/demo usage
    import sys

    logging.basicConfig(level=logging.INFO)

    if len(sys.argv) > 1:
        db_path = sys.argv[1]
    else:
        db_path = Path(__file__).parent / 'data' / 'bre_analysis.db'

    manager = MigrationManager(db_path)
    status = manager.get_migration_status()

    print("\n=== Migration Status ===")
    print(f"Applied: {status['total_applied']}")
    print(f"Pending: {status['total_pending']}")

    if status['pending_migrations']:
        print("\nPending migrations:")
        for m in status['pending_migrations']:
            print(f"  - {m}")

        response = input("\nApply pending migrations? (y/n): ")
        if response.lower() == 'y':
            results = manager.apply_all_pending()
            print(f"\nApplied {results['applied']} migrations")
            if results['errors']:
                print("Errors:", results['errors'])
