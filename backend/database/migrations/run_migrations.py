"""
Migration Runner
Executes SQL migration scripts in order
"""
import sqlite3
from pathlib import Path
import sys

def run_migrations(db_path: str):
    """Run all pending migrations"""

    migrations_dir = Path(__file__).parent
    db_file = Path(db_path)

    if not db_file.exists():
        print(f"Error: Database not found at {db_path}")
        return False

    print(f"Running migrations on database: {db_path}")

    # Connect to database
    conn = sqlite3.connect(db_path)
    cursor = conn.cursor()

    # Check current schema version
    try:
        cursor.execute("SELECT MAX(version) FROM schema_version")
        current_version = cursor.fetchone()[0] or 0
    except sqlite3.OperationalError:
        current_version = 0
        print("Warning: schema_version table not found, assuming version 0")

    print(f"Current schema version: {current_version}")

    # Find migration files
    migration_files = sorted(migrations_dir.glob("*.sql"))

    if not migration_files:
        print("No migration files found")
        return True

    # Execute migrations in order
    migrations_run = 0
    for migration_file in migration_files:
        # Extract version from filename (e.g., 001_enhanced_dependencies.sql -> 1)
        try:
            file_version = int(migration_file.stem.split('_')[0])
        except (ValueError, IndexError):
            print(f"Warning: Skipping {migration_file.name} - invalid naming format")
            continue

        # Skip if already applied
        if file_version <= current_version:
            print(f"  Skipping {migration_file.name} (already applied)")
            continue

        print(f"  Applying {migration_file.name}...")

        try:
            # Read migration SQL
            with open(migration_file, 'r', encoding='utf-8') as f:
                migration_sql = f.read()

            # Execute migration (split by semicolon to handle multiple statements)
            cursor.executescript(migration_sql)
            conn.commit()

            migrations_run += 1
            print(f"    ✓ Successfully applied {migration_file.name}")

        except sqlite3.Error as e:
            print(f"    ✗ Error applying {migration_file.name}: {e}")
            conn.rollback()
            return False

    # Final schema version check
    cursor.execute("SELECT MAX(version) FROM schema_version")
    new_version = cursor.fetchone()[0] or 0

    conn.close()

    print(f"\nMigrations complete!")
    print(f"  Schema version: {current_version} -> {new_version}")
    print(f"  Migrations applied: {migrations_run}")

    return True


if __name__ == "__main__":
    if len(sys.argv) > 1:
        db_path = sys.argv[1]
    else:
        # Default database path
        db_path = Path(__file__).parent.parent / "data" / "bre_analysis.db"

    success = run_migrations(str(db_path))
    sys.exit(0 if success else 1)
