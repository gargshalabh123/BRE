"""
Test script to verify CodeAnalyzer integration with specialized analyzers
"""
import sys
from pathlib import Path

# Add backend to path
sys.path.insert(0, str(Path(__file__).parent / 'backend'))

from analyzers.code_analyzer import CodeAnalyzer

def test_cobol_analysis():
    """Test COBOL analysis with specialized analyzer"""

    # Test with the uploaded COBOL project
    test_path = r"C:\code\BRE\uploads\tmpu1j906kt\extracted\aws-mainframe-modernization-carddemo-main\app"

    print("=" * 80)
    print("Testing CodeAnalyzer with COBOL files")
    print("=" * 80)
    print(f"Analyzing: {test_path}\n")

    # Create analyzer
    analyzer = CodeAnalyzer(test_path)

    # Run full analysis
    print("Running full analysis...")
    results = analyzer.analyze_all()

    # Display results
    print("\n" + "=" * 80)
    print("SUMMARY")
    print("=" * 80)
    print(f"Total files: {results['summary']['total_files']}")
    print(f"File types: {results['summary']['file_types']}")

    print("\n" + "=" * 80)
    print("DATABASE OPERATIONS")
    print("=" * 80)
    db_ops = results['database_operations']
    print(f"Total database operations: {db_ops['total_count']}")
    print(f"By type: {db_ops['by_type']}")

    if db_ops['queries']:
        print("\nFirst 5 database operations:")
        for i, query in enumerate(db_ops['queries'][:5], 1):
            print(f"\n{i}. File: {query['file']}")
            print(f"   Type: {query['type']}")
            print(f"   Line: {query['line']}")
            print(f"   Category: {query.get('category', 'N/A')}")
            print(f"   Statement: {query['query'][:100]}...")

    print("\n" + "=" * 80)
    print("BUSINESS RULES")
    print("=" * 80)
    rules = results['business_rules']
    print(f"Total business rules found: {len(rules)}")

    if rules:
        print("\nFirst 5 business rules:")
        for i, rule in enumerate(rules[:5], 1):
            print(f"\n{i}. File: {rule['file']}")
            print(f"   Type: {rule['type']}")
            print(f"   Line: {rule['line']}")
            print(f"   Code: {rule['code'][:80]}...")

    print("\n" + "=" * 80)
    print("DEPENDENCIES")
    print("=" * 80)
    deps = results['dependencies']
    print(f"Files with dependencies: {len(deps)}")

    if deps:
        print("\nFirst 5 files with dependencies:")
        for i, (file, dep_list) in enumerate(list(deps.items())[:5], 1):
            print(f"\n{i}. {file}")
            print(f"   Dependencies: {', '.join(dep_list[:5])}")
            if len(dep_list) > 5:
                print(f"   ... and {len(dep_list) - 5} more")

    print("\n" + "=" * 80)
    print("TEST COMPLETE")
    print("=" * 80)

    # Verify we got results
    success = (
        db_ops['total_count'] > 0 or
        len(rules) > 0 or
        len(deps) > 0
    )

    if success:
        print("[SUCCESS] Analysis returned results!")
        print(f"  - Database operations: {db_ops['total_count']}")
        print(f"  - Business rules: {len(rules)}")
        print(f"  - Dependencies: {len(deps)}")
    else:
        print("[FAILURE] No results found!")

    return success

if __name__ == "__main__":
    try:
        success = test_cobol_analysis()
        sys.exit(0 if success else 1)
    except Exception as e:
        print(f"\n[ERROR] {e}")
        import traceback
        traceback.print_exc()
        sys.exit(1)
