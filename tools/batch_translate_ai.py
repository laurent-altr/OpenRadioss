#!/usr/bin/env python3
"""
AI-Assisted Batch Translation for Fortran Comments

This script helps translate French comments using AI services or LLM APIs.
It processes comments in batches and allows for human review before applying.

Requirements:
    pip install openai anthropic  # or other AI service client

Usage:
    # Using OpenAI
    export OPENAI_API_KEY=your_key
    python batch_translate_ai.py --input french_comments_report.json --provider openai

    # Using Anthropic Claude
    export ANTHROPIC_API_KEY=your_key
    python batch_translate_ai.py --input french_comments_report.json --provider anthropic

    # Review mode (no AI, just format for manual translation)
    python batch_translate_ai.py --input french_comments_report.json --review-only
"""

import json
import argparse
import os
import time
from pathlib import Path
from typing import List, Dict

# Optional imports
try:
    import openai
    HAS_OPENAI = True
except ImportError:
    HAS_OPENAI = False

try:
    import anthropic
    HAS_ANTHROPIC = True
except ImportError:
    HAS_ANTHROPIC = False


class AITranslator:
    """Handles AI-powered translation of technical French comments"""

    def __init__(self, provider='openai', batch_size=10):
        self.provider = provider
        self.batch_size = batch_size

        if provider == 'openai' and HAS_OPENAI:
            self.client = openai.OpenAI(api_key=os.getenv('OPENAI_API_KEY'))
        elif provider == 'anthropic' and HAS_ANTHROPIC:
            self.client = anthropic.Anthropic(api_key=os.getenv('ANTHROPIC_API_KEY'))
        else:
            self.client = None

    def create_translation_prompt(self, comments: List[str]) -> str:
        """Create a prompt for translating technical comments"""
        comments_text = '\n'.join([f"{i+1}. {c}" for i, c in enumerate(comments)])

        return f"""Translate these Fortran code comments from French to English.
These are technical comments from a finite element analysis software (OpenRadioss).
Maintain technical accuracy and keep the same tone and brevity.

French comments:
{comments_text}

Provide ONLY the English translations, numbered 1-{len(comments)}, without explanations."""

    def translate_batch_openai(self, comments: List[str]) -> List[str]:
        """Translate a batch using OpenAI"""
        prompt = self.create_translation_prompt(comments)

        response = self.client.chat.completions.create(
            model="gpt-4",
            messages=[
                {"role": "system", "content": "You are a technical translator specializing in computational mechanics and Fortran code."},
                {"role": "user", "content": prompt}
            ],
            temperature=0.3
        )

        result = response.choices[0].message.content
        translations = self.parse_numbered_translations(result, len(comments))
        return translations

    def translate_batch_anthropic(self, comments: List[str]) -> List[str]:
        """Translate a batch using Anthropic Claude"""
        prompt = self.create_translation_prompt(comments)

        response = self.client.messages.create(
            model="claude-3-5-sonnet-20241022",
            max_tokens=4096,
            messages=[{"role": "user", "content": prompt}],
            temperature=0.3
        )

        result = response.content[0].text
        translations = self.parse_numbered_translations(result, len(comments))
        return translations

    def parse_numbered_translations(self, text: str, expected_count: int) -> List[str]:
        """Parse numbered translations from AI response"""
        lines = text.strip().split('\n')
        translations = []

        for line in lines:
            # Match patterns like "1. Translation" or "1) Translation"
            if line.strip() and (line[0].isdigit() or line.strip()[0].isdigit()):
                # Remove numbering
                parts = line.split('.', 1) if '.' in line else line.split(')', 1)
                if len(parts) == 2:
                    translations.append(parts[1].strip())

        # Fallback if parsing fails
        if len(translations) != expected_count:
            print(f"Warning: Expected {expected_count} translations, got {len(translations)}")
            return [text] * expected_count

        return translations

    def translate_file_comments(self, file_info: Dict, progress_callback=None) -> Dict:
        """Translate all comments in a file"""
        comments = file_info['comments']
        translated_comments = []

        # Process in batches
        for i in range(0, len(comments), self.batch_size):
            batch = comments[i:i+self.batch_size]
            batch_texts = [c['text'] for c in batch]

            if self.provider == 'openai':
                translations = self.translate_batch_openai(batch_texts)
            elif self.provider == 'anthropic':
                translations = self.translate_batch_anthropic(batch_texts)
            else:
                translations = [''] * len(batch_texts)

            for comment, translation in zip(batch, translations):
                translated_comments.append({
                    'line': comment['line'],
                    'original': comment['text'],
                    'translation': translation,
                    'full_line': comment['full_line']
                })

            if progress_callback:
                progress_callback(i + len(batch), len(comments))

            # Rate limiting
            time.sleep(1)

        return {
            'file': file_info['file'],
            'comments': translated_comments
        }


def create_review_html(report_file, output_file='review.html'):
    """Create an HTML file for easier review of translations"""
    with open(report_file, 'r', encoding='utf-8') as f:
        data = json.load(f)

    html = """<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <title>OpenRadioss Comment Translation Review</title>
    <style>
        body { font-family: Arial, sans-serif; margin: 20px; background: #f5f5f5; }
        .file { background: white; margin: 20px 0; padding: 15px; border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }
        .file-header { font-weight: bold; color: #2c3e50; font-size: 1.1em; margin-bottom: 10px; }
        .comment { margin: 10px 0; padding: 10px; background: #ecf0f1; border-left: 4px solid #3498db; }
        .line-num { color: #7f8c8d; font-size: 0.9em; }
        .french { color: #e74c3c; margin: 5px 0; }
        .english { color: #27ae60; margin: 5px 0; }
        .stats { background: #3498db; color: white; padding: 15px; border-radius: 5px; margin-bottom: 20px; }
        textarea { width: 100%; height: 60px; margin-top: 5px; padding: 5px; }
        button { background: #27ae60; color: white; border: none; padding: 8px 15px; cursor: pointer; border-radius: 3px; }
        button:hover { background: #229954; }
    </style>
</head>
<body>
    <div class="stats">
        <h1>OpenRadioss Comment Translation Review</h1>
        <p>Files with French comments: """ + str(data['summary']['files_with_french']) + """</p>
        <p>Total French comments: """ + str(data['summary']['total_french_comments']) + """</p>
    </div>
"""

    for file_info in data['files'][:50]:  # Limit to first 50 files for HTML
        html += f"""
    <div class="file">
        <div class="file-header">📄 {file_info['file']}</div>
        <div>Comments: {file_info['comment_count']}</div>
"""

        for comment in file_info['comments']:
            html += f"""
        <div class="comment">
            <div class="line-num">Line {comment['line']}</div>
            <div class="french">🇫🇷 {comment['text']}</div>
            <div class="english">🇬🇧 <em>[Translation needed]</em></div>
        </div>
"""

        html += "    </div>\n"

    html += """
</body>
</html>
"""

    with open(output_file, 'w', encoding='utf-8') as f:
        f.write(html)

    print(f"HTML review file created: {output_file}")
    print(f"Open it in a browser to review comments visually")


def main():
    parser = argparse.ArgumentParser(description='AI-Assisted Comment Translation')
    parser.add_argument('--input', required=True, help='Input report JSON file')
    parser.add_argument('--output', default='translations_ai.json', help='Output translations file')
    parser.add_argument('--provider', choices=['openai', 'anthropic'], default='openai',
                       help='AI provider to use')
    parser.add_argument('--batch-size', type=int, default=10, help='Comments per API call')
    parser.add_argument('--review-only', action='store_true', help='Generate review files without AI translation')
    parser.add_argument('--html', action='store_true', help='Generate HTML review file')
    parser.add_argument('--limit', type=int, help='Limit number of files to process')

    args = parser.parse_args()

    if args.html or args.review_only:
        create_review_html(args.input)
        return

    # Load report
    with open(args.input, 'r', encoding='utf-8') as f:
        data = json.load(f)

    # Check API keys
    if args.provider == 'openai' and not os.getenv('OPENAI_API_KEY'):
        print("Error: OPENAI_API_KEY environment variable not set")
        return
    if args.provider == 'anthropic' and not os.getenv('ANTHROPIC_API_KEY'):
        print("Error: ANTHROPIC_API_KEY environment variable not set")
        return

    # Initialize translator
    translator = AITranslator(provider=args.provider, batch_size=args.batch_size)

    # Process files
    translated_files = []
    files_to_process = data['files'][:args.limit] if args.limit else data['files']

    print(f"Processing {len(files_to_process)} files with {args.provider}...\n")

    for idx, file_info in enumerate(files_to_process, 1):
        print(f"[{idx}/{len(files_to_process)}] {file_info['file']} ({file_info['comment_count']} comments)")

        def progress(current, total):
            print(f"  Progress: {current}/{total} comments", end='\r')

        translated = translator.translate_file_comments(file_info, progress_callback=progress)
        translated_files.append(translated)
        print()  # New line after progress

    # Save results
    output = {
        'instructions': 'Review translations and edit as needed before applying',
        'files': translated_files
    }

    with open(args.output, 'w', encoding='utf-8') as f:
        json.dump(output, f, indent=2, ensure_ascii=False)

    print(f"\n✓ Translations saved to: {args.output}")
    print(f"\nNext steps:")
    print(f"1. Review and edit translations in {args.output}")
    print(f"2. Apply: python comment_translator.py apply {args.output}")


if __name__ == '__main__':
    main()
