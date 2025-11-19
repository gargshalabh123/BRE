"""
Comprehensive diagnostic script to identify the issue
"""
import sys
from pathlib import Path
import subprocess

print("=" * 80)
print("DIAGNOSTIC SCRIPT - Finding Why You're Seeing Zeros")
print("=" * 80)
print()

# Check 1: Verify code changes are present
print("[1] Checking if code changes are in place...")
code_file = Path("backend/analyzers/code_analyzer.py")
if code_file.exists():
    content = code_file.read_text()
    if "language_router" in content and "_get_specialized_analysis" in content:
        print("    ✓ Code changes are present in code_analyzer.py")
    else:
        print("    ✗ Code changes are MISSING! Need to re-apply fix.")
        sys.exit(1)
else:
    print("    ✗ code_analyzer.py not found!")
    sys.exit(1)

print()

# Check 2: Test if the code works standalone
print("[2] Testing if analysis works (standalone)...")
sys.path.insert(0, str(Path(__file__).parent / 'backend'))
try:
    from analyzers.code_analyzer import CodeAnalyzer

    test_path = r"C:\code\BRE\uploads\tmpu1j906kt\extracted"
    if Path(test_path).exists():
        analyzer = CodeAnalyzer(test_path)
        analyzer.scan_directory()
        db_ops = analyzer._extract_database_operations()

        if db_ops['total_count'] > 0:
            print(f"    ✓ Standalone test WORKS: {db_ops['total_count']} DB operations found")
        else:
            print(f"    ✗ Standalone test FAILED: 0 DB operations")
    else:
        print(f"    ⚠ Test path doesn't exist: {test_path}")
except Exception as e:
    print(f"    ✗ Error: {e}")

print()

# Check 3: Verify which uploads have data
print("[3] Checking which uploads have COBOL files...")
upload_dir = Path(r"C:\code\BRE\uploads")
uploads_with_data = []

for upload_path in upload_dir.iterdir():
    if not upload_path.is_dir():
        continue

    extract_dir = upload_path / "extracted"
    if extract_dir.exists():
        cobol_files = list(extract_dir.rglob("*.cbl")) + list(extract_dir.rglob("*.CBL"))
        if cobol_files:
            uploads_with_data.append(upload_path.name)

if uploads_with_data:
    print(f"    ✓ Found {len(uploads_with_data)} uploads with COBOL files:")
    for uid in uploads_with_data:
        print(f"      - {uid}")
else:
    print("    ✗ NO uploads with COBOL files found!")
    print("      You need to upload a COBOL zip file")

print()

# Check 4: Try to connect to backend
print("[4] Checking if backend server is running...")
try:
    import requests
    try:
        response = requests.get("http://localhost:8000/health", timeout=2)
        if response.status_code == 200:
            print("    ✓ Backend server is RUNNING on http://localhost:8000")

            # Check 5: Test API endpoint
            print()
            print("[5] Testing API endpoint with real upload...")
            if uploads_with_data:
                test_id = uploads_with_data[0]
                print(f"    Using upload_id: {test_id}")

                api_response = requests.post(
                    f"http://localhost:8000/api/analysis/{test_id}/full",
                    timeout=30
                )

                if api_response.status_code == 200:
                    data = api_response.json()
                    db_count = data.get('database_operations', {}).get('total_count', 0)
                    rules_count = len(data.get('business_rules', []))
                    deps_count = len(data.get('dependencies', {}))

                    print(f"    API Response:")
                    print(f"      - Database operations: {db_count}")
                    print(f"      - Business rules: {rules_count}")
                    print(f"      - Dependencies: {deps_count}")

                    if db_count > 0 or rules_count > 0 or deps_count > 0:
                        print()
                        print("    ✓ API IS WORKING! Returning data correctly.")
                    else:
                        print()
                        print("    ✗ API is running but returning ZERO data")
                        print("      Possible cause: Backend needs restart")
                else:
                    print(f"    ✗ API error: {api_response.status_code}")
                    print(f"      {api_response.text}")
        else:
            print(f"    ✗ Backend health check failed: {response.status_code}")
    except requests.exceptions.ConnectionError:
        print("    ✗ Backend server is NOT RUNNING")
        print("      Start it with: cd backend && python main.py")
    except requests.exceptions.Timeout:
        print("    ✗ Backend server timed out")
except ImportError:
    print("    ⚠ 'requests' module not installed, skipping API test")

print()
print("=" * 80)
print("DIAGNOSIS SUMMARY")
print("=" * 80)

print()
print("If you see zeros in the frontend:")
print()
print("1. Make sure backend is RUNNING:")
print("   cd backend && python main.py")
print()
print("2. Use one of these upload IDs:")
for uid in uploads_with_data[:2]:
    print(f"   - {uid}")
print()
print("3. Or upload a NEW COBOL zip file with backend running")
print()
print("4. Hard refresh your browser (Ctrl+F5)")
print()
