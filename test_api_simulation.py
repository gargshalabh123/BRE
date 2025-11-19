"""
Simulate the exact API call flow to debug the issue
"""
import sys
from pathlib import Path

# Add backend to path
sys.path.insert(0, str(Path(__file__).parent / 'backend'))

from analyzers.code_analyzer import CodeAnalyzer

def simulate_api_call(upload_id: str):
    """Simulate the exact flow from the API endpoint"""

    UPLOAD_DIR = Path(r"C:\code\BRE\uploads")
    upload_path = UPLOAD_DIR / upload_id

    print(f"Upload path: {upload_path}")
    print(f"Upload path exists: {upload_path.exists()}")

    if not upload_path.exists():
        print(f"ERROR: Upload not found!")
        return

    # This is the exact logic from api/analysis.py
    extract_dir = upload_path / "extracted"
    analysis_path = extract_dir if extract_dir.exists() else upload_path

    print(f"Extract dir: {extract_dir}")
    print(f"Extract dir exists: {extract_dir.exists()}")
    print(f"Analysis path: {analysis_path}")
    print()

    # Create analyzer exactly like the API does
    analyzer = CodeAnalyzer(str(analysis_path))

    print("Scanning directory...")
    analyzer.scan_directory()
    print(f"Files found: {len(analyzer.files)}")

    # Count COBOL files
    cobol_files = [f for f in analyzer.files if f.suffix.lower() in ['.cbl', '.cob', '.cobol']]
    print(f"COBOL files: {len(cobol_files)}")
    if cobol_files:
        print(f"First COBOL file: {cobol_files[0]}")
    print()

    # Test database operations extraction
    print("=" * 80)
    print("DATABASE OPERATIONS")
    print("=" * 80)
    db_ops = analyzer._extract_database_operations()
    print(f"Total: {db_ops['total_count']}")
    print(f"By type: {db_ops['by_type']}")

    if db_ops['queries'][:3]:
        print("\nFirst 3 operations:")
        for i, op in enumerate(db_ops['queries'][:3], 1):
            print(f"{i}. {op['file']} (line {op['line']}): {op['type']}")

    print()

    # Test business rules extraction
    print("=" * 80)
    print("BUSINESS RULES")
    print("=" * 80)
    rules = analyzer._extract_business_rules()
    print(f"Total: {len(rules)}")

    if rules[:3]:
        print("\nFirst 3 rules:")
        for i, rule in enumerate(rules[:3], 1):
            print(f"{i}. {rule['file']} (line {rule['line']}): {rule['type']}")

    print()

    # Test dependencies
    print("=" * 80)
    print("DEPENDENCIES")
    print("=" * 80)
    deps = analyzer._analyze_dependencies()
    print(f"Files with dependencies: {len(deps)}")

    if deps:
        first_file = list(deps.keys())[0]
        print(f"\nFirst file: {first_file}")
        print(f"Dependencies: {deps[first_file][:5]}")

if __name__ == "__main__":
    # Test with the old upload that has extracted files
    upload_id = "tmpu1j906kt"

    print("=" * 80)
    print(f"SIMULATING API CALL FOR UPLOAD: {upload_id}")
    print("=" * 80)
    print()

    simulate_api_call(upload_id)
