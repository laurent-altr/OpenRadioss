#!/usr/bin/env python3
"""
Fortran Comment Translation Helper for OpenRadioss

This tool helps identify, extract, and translate French comments in Fortran files.
It provides an efficient workflow for reviewing thousands of files without manual editing.

Usage:
    # Scan for French comments
    python comment_translator.py scan

    # Generate detailed report
    python comment_translator.py report -o french_comments_report.json

    # Interactive review mode
    python comment_translator.py review

    # Apply translations from a reviewed file
    python comment_translator.py apply translations.json
"""

import re
import os
import json
import argparse
from pathlib import Path
from typing import List, Dict, Tuple
from collections import defaultdict

# Common French words in technical comments
FRENCH_INDICATORS = [
    'calcul', 'avec', 'pour', 'dans', 'sans', 'pression',
    'repere', 'armature', 'beton', 'vitesse', 'deformation',
    'contrainte', 'effort', 'noeud', 'element', 'masse',
    'suivant', 'selon', 'entre', 'tous', 'chaque',
    'initial', 'finale', 'temps', 'iteration', 'algorithme',
    'retour', 'sortie', 'entree', 'valeur', 'variable',
    'matrice', 'vecteur', 'tableau', 'indice', 'numero'
]

# Fortran comment patterns
COMMENT_PATTERNS = [
    re.compile(r'^[Cc!\*]\s*(.+)$'),  # Standard Fortran comments
    re.compile(r'^\s*!\s*(.+)$'),     # Free-form comments
]


class FortranCommentAnalyzer:
    """Analyzes Fortran files for French comments"""

    def __init__(self, root_dir='.'):
        self.root_dir = Path(root_dir)
        self.results = []

    def is_likely_french(self, text: str) -> Tuple[bool, List[str]]:
        """Check if text is likely French, return (bool, matched_words)"""
        text_lower = text.lower()
        matched = []

        for word in FRENCH_INDICATORS:
            if re.search(r'\b' + word + r'\b', text_lower):
                matched.append(word)

        # Consider it French if 2+ French indicators found
        return len(matched) >= 2, matched

    def extract_comments(self, filepath: Path) -> List[Dict]:
        """Extract all comments from a Fortran file"""
        comments = []

        try:
            with open(filepath, 'r', encoding='latin-1') as f:
                for line_num, line in enumerate(f, 1):
                    for pattern in COMMENT_PATTERNS:
                        match = pattern.match(line)
                        if match:
                            comment_text = match.group(1).strip()
                            if comment_text and not comment_text.startswith('-'):
                                is_french, french_words = self.is_likely_french(comment_text)
                                if is_french:
                                    comments.append({
                                        'line': line_num,
                                        'text': comment_text,
                                        'full_line': line.rstrip(),
                                        'french_words': french_words
                                    })
                            break
        except Exception as e:
            print(f"Error reading {filepath}: {e}")

        return comments

    def scan_files(self, pattern='**/*.F') -> Dict:
        """Scan all Fortran files for French comments"""
        print(f"Scanning {self.root_dir} for {pattern}...")

        files_with_french = []
        total_files = 0
        total_french_comments = 0

        for filepath in self.root_dir.glob(pattern):
            total_files += 1
            if total_files % 100 == 0:
                print(f"  Processed {total_files} files...", end='\r')

            comments = self.extract_comments(filepath)
            if comments:
                rel_path = filepath.relative_to(self.root_dir)
                files_with_french.append({
                    'file': str(rel_path),
                    'abs_path': str(filepath),
                    'comment_count': len(comments),
                    'comments': comments
                })
                total_french_comments += len(comments)

        print(f"\n\nScan complete!")
        print(f"  Total files scanned: {total_files}")
        print(f"  Files with French comments: {len(files_with_french)}")
        print(f"  Total French comments found: {total_french_comments}")

        return {
            'summary': {
                'total_files_scanned': total_files,
                'files_with_french': len(files_with_french),
                'total_french_comments': total_french_comments
            },
            'files': files_with_french
        }

    def generate_report(self, output_file='french_comments_report.json'):
        """Generate a detailed JSON report"""
        results = self.scan_files()

        with open(output_file, 'w', encoding='utf-8') as f:
            json.dump(results, f, indent=2, ensure_ascii=False)

        print(f"\nReport saved to: {output_file}")
        return results

    def generate_review_file(self, output_file='comments_to_review.txt'):
        """Generate a human-readable file for review"""
        results = self.scan_files()

        with open(output_file, 'w', encoding='utf-8') as f:
            f.write("=" * 80 + "\n")
            f.write("FRENCH COMMENTS REVIEW - OpenRadioss\n")
            f.write("=" * 80 + "\n\n")
            f.write(f"Total files with French comments: {results['summary']['files_with_french']}\n")
            f.write(f"Total French comments: {results['summary']['total_french_comments']}\n\n")

            # Group by directory
            by_dir = defaultdict(list)
            for file_info in results['files']:
                dir_name = str(Path(file_info['file']).parent)
                by_dir[dir_name].append(file_info)

            for dir_name in sorted(by_dir.keys()):
                f.write(f"\n{'=' * 80}\n")
                f.write(f"Directory: {dir_name}\n")
                f.write(f"{'=' * 80}\n\n")

                for file_info in by_dir[dir_name]:
                    f.write(f"\nFile: {file_info['file']}\n")
                    f.write(f"French comments: {file_info['comment_count']}\n")
                    f.write("-" * 80 + "\n")

                    for comment in file_info['comments']:
                        f.write(f"  Line {comment['line']:5d}: {comment['text']}\n")
                    f.write("\n")

        print(f"\nReview file saved to: {output_file}")
        return output_file


class CommentTranslator:
    """Helper for translating comments with AI/manual review"""

    def __init__(self, report_file='french_comments_report.json'):
        with open(report_file, 'r', encoding='utf-8') as f:
            self.data = json.load(f)

    def create_translation_template(self, output_file='translations_template.json'):
        """Create a template file for translations"""
        template = {
            'instructions': 'Fill in the "translation" field for each comment. Leave blank to skip.',
            'files': []
        }

        for file_info in self.data['files']:
            file_entry = {
                'file': file_info['file'],
                'comments': []
            }

            for comment in file_info['comments']:
                file_entry['comments'].append({
                    'line': comment['line'],
                    'original': comment['text'],
                    'translation': '',  # To be filled
                    'full_line': comment['full_line']
                })

            template['files'].append(file_entry)

        with open(output_file, 'w', encoding='utf-8') as f:
            json.dump(template, f, indent=2, ensure_ascii=False)

        print(f"Translation template created: {output_file}")
        print(f"\nNext steps:")
        print(f"1. Edit {output_file} and fill in translations")
        print(f"2. Run: python comment_translator.py apply {output_file}")
        return output_file

    def apply_translations(self, translation_file):
        """Apply translations from a filled template"""
        with open(translation_file, 'r', encoding='utf-8') as f:
            translations = json.load(f)

        print("Applying translations...\n")

        for file_entry in translations['files']:
            filepath = Path(file_entry['file'])
            if not filepath.exists():
                filepath = Path(self.data['files'][0]['abs_path']).parent.parent / file_entry['file']

            # Read original file
            with open(filepath, 'r', encoding='latin-1') as f:
                lines = f.readlines()

            # Apply translations
            changes = 0
            for comment in file_entry['comments']:
                if comment['translation']:
                    line_idx = comment['line'] - 1
                    original_line = lines[line_idx]

                    # Preserve the comment marker and indentation
                    match = re.match(r'^([Cc!\*\s]*)', original_line)
                    if match:
                        prefix = match.group(1)
                        new_line = f"{prefix}{comment['translation']}\n"
                        lines[line_idx] = new_line
                        changes += 1

            if changes > 0:
                # Write back
                with open(filepath, 'w', encoding='latin-1') as f:
                    f.writelines(lines)
                print(f"✓ {file_entry['file']}: {changes} comments translated")

        print(f"\nTranslation complete!")


def main():
    parser = argparse.ArgumentParser(description='Fortran Comment Translation Helper')
    parser.add_argument('command', choices=['scan', 'report', 'review', 'template', 'apply'],
                       help='Command to execute')
    parser.add_argument('-o', '--output', help='Output file name')
    parser.add_argument('-i', '--input', help='Input file (for apply command)')
    parser.add_argument('--root', default='.', help='Root directory to scan')

    args = parser.parse_args()

    if args.command == 'scan':
        analyzer = FortranCommentAnalyzer(args.root)
        results = analyzer.scan_files()
        print(f"\nFound {results['summary']['files_with_french']} files with French comments")

    elif args.command == 'report':
        analyzer = FortranCommentAnalyzer(args.root)
        output = args.output or 'french_comments_report.json'
        analyzer.generate_report(output)

    elif args.command == 'review':
        analyzer = FortranCommentAnalyzer(args.root)
        output = args.output or 'comments_to_review.txt'
        analyzer.generate_review_file(output)

    elif args.command == 'template':
        report_file = args.input or 'french_comments_report.json'
        translator = CommentTranslator(report_file)
        output = args.output or 'translations_template.json'
        translator.create_translation_template(output)

    elif args.command == 'apply':
        if not args.input:
            print("Error: --input required for apply command")
            return
        translator = CommentTranslator()
        translator.apply_translations(args.input)


if __name__ == '__main__':
    main()
