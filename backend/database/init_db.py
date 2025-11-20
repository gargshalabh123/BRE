"""
Database initialization script
Run this to create the database and initialize with default data
"""
from db_manager import DatabaseManager
import sys


def initialize_database():
    """Initialize the database with schema and default data"""
    print("=" * 60)
    print("Business Rules Extraction - Database Initialization")
    print("=" * 60)

    try:
        # Create database manager (this will create database if it doesn't exist)
        db = DatabaseManager()

        print("\n[OK] Database created successfully at:", db.db_path)

        # Verify schema version
        cursor = db.connection.cursor()
        cursor.execute("SELECT version, applied_at, description FROM schema_version")
        version = cursor.fetchone()

        if version:
            print(f"[OK] Schema version: {version[0]}")
            print(f"     Applied at: {version[1]}")
            print(f"     Description: {version[2]}")

        # Check default roles
        cursor.execute("SELECT role_name, description FROM roles ORDER BY id")
        roles = cursor.fetchall()

        print(f"\n[OK] Default roles created: {len(roles)}")
        for role in roles:
            print(f"     - {role[0]}: {role[1]}")

        # Check default user
        cursor.execute("SELECT username, email, role_id FROM users")
        users = cursor.fetchall()

        print(f"\n[OK] Default users created: {len(users)}")
        for user in users:
            print(f"     - {user[0]} ({user[1]})")

        print("\n" + "=" * 60)
        print("Database initialization completed successfully!")
        print("=" * 60)
        print("\nDefault admin credentials:")
        print("  Username: admin")
        print("  Password: admin123")
        print("\n[WARNING] IMPORTANT: Change the default password in production!")
        print("=" * 60)

        db.close()
        return True

    except Exception as e:
        print(f"\n[ERROR] Database initialization failed!")
        print(f"        {str(e)}")
        import traceback
        traceback.print_exc()
        return False


if __name__ == "__main__":
    success = initialize_database()
    sys.exit(0 if success else 1)
