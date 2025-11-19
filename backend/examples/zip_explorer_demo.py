"""
ZIP Explorer Demo Script

This script demonstrates how to use the ZIP Explorer functionality
to analyze a codebase ZIP file.
"""
import sys
from pathlib import Path

# Add parent directory to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent))

from utils.zip_explorer import ZipExplorer, explore_zip, get_zip_summary
import json


def print_separator(title=""):
    """Print a visual separator"""
    print("\n" + "=" * 80)
    if title:
        print(f"  {title}")
        print("=" * 80)
    print()


def demo_quick_summary(zip_path: str):
    """Demonstrate quick summary functionality"""
    print_separator("QUICK SUMMARY")

    summary = get_zip_summary(zip_path)

    print(f"Filename: {summary['filename']}")
    print(f"Total Files: {summary['total_files']}")
    print(f"Total Size: {summary['total_size_mb']} MB")
    print(f"Total Lines of Code: {summary['total_loc']:,}")

    print("\nTop Languages:")
    for lang in summary['languages']:
        print(f"  - {lang['language']}: {lang['file_count']} files, "
              f"{lang['total_loc']:,} LOC ({lang['percentage']}%)")

    print("\nTop Extensions:")
    for ext, data in summary['top_extensions']:
        print(f"  - {ext}: {data['count']} files, {data['loc']:,} LOC")


def demo_basic_exploration(zip_path: str):
    """Demonstrate basic exploration without extraction"""
    print_separator("BASIC EXPLORATION")

    explorer = ZipExplorer(zip_path)
    results = explorer.explore(extract=False)

    # ZIP Info
    print("ZIP File Information:")
    zip_info = results['zip_info']
    print(f"  Path: {zip_info['path']}")
    print(f"  Size: {zip_info['size_mb']} MB")
    print(f"  Created: {zip_info['created']}")
    print(f"  Compression: {zip_info['compression']}")

    # Statistics
    print("\nStatistics:")
    stats = results['statistics']
    print(f"  Total Files: {stats['total_files']}")
    print(f"  Total LOC: {stats['total_lines_of_code']:,}")
    print(f"  Text Files: {stats['text_files']}")
    print(f"  Binary Files: {stats['binary_files']}")
    print(f"  Compression Ratio: {stats['overall_compression_ratio']}%")
    print(f"  Unique Directories: {stats['unique_directories']}")
    print(f"  Unique Extensions: {stats['unique_extensions']}")

    # Language Distribution
    print("\nLanguage Distribution:")
    lang_dist = results['language_distribution']['languages_sorted']
    for lang_info in lang_dist[:5]:  # Top 5 languages
        print(f"  {lang_info['language']}: "
              f"{lang_info['file_count']} files, "
              f"{lang_info['total_loc']:,} LOC, "
              f"{lang_info['percentage']}%")

    # Largest Files
    print("\nLargest Files:")
    for file_info in stats['largest_files'][:5]:  # Top 5
        print(f"  {file_info['path']}: {file_info['size_kb']} KB, {file_info['loc']:,} LOC")


def demo_file_filtering(zip_path: str):
    """Demonstrate file filtering capabilities"""
    print_separator("FILE FILTERING")

    explorer = ZipExplorer(zip_path)
    results = explorer.explore(extract=False)

    # Filter COBOL files
    cobol_files = [f for f in results['files'] if f['language'] == 'cobol']
    print(f"COBOL Files: {len(cobol_files)}")
    if cobol_files:
        print("  Sample files:")
        for f in cobol_files[:5]:
            print(f"    - {f['path']} ({f['size_kb']} KB, {f['estimated_loc']} LOC)")

    # Filter SQL files
    sql_files = [f for f in results['files'] if f['language'] == 'sql']
    print(f"\nSQL Files: {len(sql_files)}")
    if sql_files:
        print("  Sample files:")
        for f in sql_files[:5]:
            print(f"    - {f['path']} ({f['size_kb']} KB, {f['estimated_loc']} LOC)")

    # Filter AS400 files
    as400_files = [f for f in results['files'] if f['language'] == 'as400']
    print(f"\nAS400/RPG Files: {len(as400_files)}")
    if as400_files:
        print("  Sample files:")
        for f in as400_files[:5]:
            print(f"    - {f['path']} ({f['size_kb']} KB, {f['estimated_loc']} LOC)")


def demo_file_tree(zip_path: str):
    """Demonstrate file tree structure"""
    print_separator("FILE TREE STRUCTURE")

    explorer = ZipExplorer(zip_path)
    results = explorer.explore(extract=False)

    def print_tree(node, indent=0):
        """Recursively print tree structure"""
        prefix = "  " * indent
        if node['type'] == 'folder':
            print(f"{prefix}📁 {node['name']}/")
            for child in node.get('children', [])[:10]:  # Limit depth
                print_tree(child, indent + 1)
        else:
            size = node.get('size', 0)
            size_kb = round(size / 1024, 2) if size > 0 else 0
            print(f"{prefix}📄 {node['name']} ({size_kb} KB)")

    print_tree(results['file_tree'])


def demo_directory_breakdown(zip_path: str):
    """Demonstrate directory-level analysis"""
    print_separator("DIRECTORY BREAKDOWN")

    explorer = ZipExplorer(zip_path)
    results = explorer.explore(extract=False)

    by_directory = results['statistics']['by_directory']

    # Sort by LOC
    sorted_dirs = sorted(
        by_directory.items(),
        key=lambda x: x[1]['loc'],
        reverse=True
    )

    print("Directories sorted by Lines of Code:")
    for dir_path, info in sorted_dirs[:10]:  # Top 10
        print(f"\n  {dir_path or 'root'}:")
        print(f"    Files: {info['count']}")
        print(f"    Size: {round(info['size'] / 1024, 2)} KB")
        print(f"    LOC: {info['loc']:,}")


def demo_get_file_content(zip_path: str, file_path: str = None):
    """Demonstrate getting file content"""
    print_separator("FILE CONTENT EXTRACTION")

    explorer = ZipExplorer(zip_path)
    results = explorer.explore(extract=False)

    # If no file path provided, use the first COBOL file
    if not file_path:
        cobol_files = [f for f in results['files'] if f['language'] == 'cobol']
        if not cobol_files:
            print("No COBOL files found in ZIP")
            return
        file_path = cobol_files[0]['path']

    print(f"Reading file: {file_path}\n")

    try:
        content = explorer.get_file_content(file_path)
        lines = content.split('\n')

        print(f"Total lines: {len(lines)}")
        print(f"File size: {len(content)} bytes")
        print("\nFirst 20 lines:")
        print("-" * 80)
        for i, line in enumerate(lines[:20], 1):
            print(f"{i:4d} | {line}")
        print("-" * 80)
    except Exception as e:
        print(f"Error reading file: {e}")


def demo_detailed_analysis(zip_path: str):
    """Demonstrate detailed code analysis (extracts files)"""
    print_separator("DETAILED CODE ANALYSIS")

    print("⚠️  This will extract the ZIP file and perform deep analysis...")
    print("This may take some time for large codebases.\n")

    explorer = ZipExplorer(zip_path)
    results = explorer.explore(extract=True)

    if results['detailed_analysis']:
        analysis = results['detailed_analysis']
        print(f"Total Analyzed Files: {analysis['total_analyzed_files']}")

        print("\nAnalyzed Files:")
        for file_path in analysis['supported_files'][:10]:
            print(f"  - {file_path}")

        # Show sample analysis results
        if analysis['analysis_results']:
            print("\nSample Analysis Result:")
            sample = analysis['analysis_results'][0]
            print(json.dumps(sample, indent=2, default=str))
    else:
        print("No detailed analysis available")


def main():
    """Main demo function"""
    import argparse

    parser = argparse.ArgumentParser(description='ZIP Explorer Demo')
    parser.add_argument('zip_file', help='Path to ZIP file to analyze')
    parser.add_argument('--detailed', action='store_true',
                        help='Perform detailed analysis (extracts files)')
    parser.add_argument('--file', help='Specific file to read from ZIP')

    args = parser.parse_args()

    if not Path(args.zip_file).exists():
        print(f"Error: File not found: {args.zip_file}")
        return

    print_separator(f"ZIP EXPLORER DEMO - {args.zip_file}")

    try:
        # Quick summary
        demo_quick_summary(args.zip_file)

        # Basic exploration
        demo_basic_exploration(args.zip_file)

        # File filtering
        demo_file_filtering(args.zip_file)

        # File tree
        demo_file_tree(args.zip_file)

        # Directory breakdown
        demo_directory_breakdown(args.zip_file)

        # Get file content
        demo_get_file_content(args.zip_file, args.file)

        # Detailed analysis (if requested)
        if args.detailed:
            demo_detailed_analysis(args.zip_file)

        print_separator("DEMO COMPLETE")

    except Exception as e:
        print(f"\n❌ Error: {e}")
        import traceback
        traceback.print_exc()


if __name__ == "__main__":
    main()
