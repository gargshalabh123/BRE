"""
Check which uploads have COBOL files and extracted data
"""
from pathlib import Path

UPLOAD_DIR = Path(r"C:\code\BRE\uploads")

print("=" * 80)
print("CHECKING UPLOADS FOR COBOL FILES")
print("=" * 80)
print()

for upload_path in UPLOAD_DIR.iterdir():
    if not upload_path.is_dir():
        continue

    upload_id = upload_path.name
    extract_dir = upload_path / "extracted"

    # Check what's in the upload
    has_extracted = extract_dir.exists()

    # Count COBOL files
    cobol_files = []
    if has_extracted:
        cobol_files = list(extract_dir.rglob("*.cbl")) + list(extract_dir.rglob("*.CBL")) + \
                     list(extract_dir.rglob("*.cob")) + list(extract_dir.rglob("*.COB"))

    status = "[OK]" if cobol_files else "[NO DATA]"

    print(f"{status} Upload ID: {upload_id}")
    print(f"   Extracted: {has_extracted}")
    print(f"   COBOL files: {len(cobol_files)}")

    if cobol_files:
        print(f"   --> USE THIS UPLOAD ID FOR TESTING")
    print()
