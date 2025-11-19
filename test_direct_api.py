"""
Test the API endpoints directly
"""
import requests
import json

BASE_URL = "http://localhost:8000"

def test_analysis_endpoint(upload_id: str):
    """Test the analysis endpoint with a specific upload_id"""

    print("=" * 80)
    print(f"TESTING API WITH UPLOAD ID: {upload_id}")
    print("=" * 80)
    print()

    # Test full analysis endpoint
    url = f"{BASE_URL}/api/analysis/{upload_id}/full"
    print(f"Calling: {url}")

    try:
        response = requests.post(url)
        print(f"Status Code: {response.status_code}")

        if response.status_code == 200:
            data = response.json()

            print("\n" + "=" * 80)
            print("RESULTS")
            print("=" * 80)

            # Database operations
            db_ops = data.get('database_operations', {})
            print(f"\nDatabase Operations: {db_ops.get('total_count', 0)}")
            print(f"By Type: {db_ops.get('by_type', {})}")

            # Business rules
            rules = data.get('business_rules', [])
            print(f"\nBusiness Rules: {len(rules)}")

            # Dependencies
            deps = data.get('dependencies', {})
            print(f"\nDependencies: {len(deps)} files")

            # Summary
            summary = data.get('summary', {})
            print(f"\nSummary:")
            print(f"  Total Files: {summary.get('total_files', 0)}")
            print(f"  File Types: {summary.get('file_types', {})}")

            # Check if we got results
            has_results = (
                db_ops.get('total_count', 0) > 0 or
                len(rules) > 0 or
                len(deps) > 0
            )

            if has_results:
                print("\n[SUCCESS] API returned results!")
            else:
                print("\n[FAILURE] API returned zero results!")

        else:
            print(f"Error: {response.text}")

    except requests.exceptions.ConnectionError:
        print("\n[ERROR] Could not connect to backend server!")
        print("Make sure the server is running: cd backend && python main.py")
    except Exception as e:
        print(f"\n[ERROR] {e}")

if __name__ == "__main__":
    # Test with the upload that has data
    test_analysis_endpoint("tmpu1j906kt")

    print("\n" + "=" * 80)
    print("NOTE: If you got [FAILURE], the backend may not have restarted")
    print("      with the updated code. Restart with: cd backend && python main.py")
    print("=" * 80)
