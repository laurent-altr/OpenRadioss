# Comment Translation Tools

Tools for translating French comments to English in OpenRadioss Fortran codebase.

## Quick Stats

- **Total Fortran files:** 5,128
- **Files with French comments:** 275 (5.4%)
- **Total French comments to translate:** 348

## Files in This Directory

### Reports (Already Generated)
- `french_comments_report.json` - Complete scan results in JSON format
- `french_comments_review.txt` - Human-readable review file

### Tools
- `comment_translator.py` - Main tool for scanning and applying translations
- `batch_translate_ai.py` - AI-assisted batch translation
- `TRANSLATION_WORKFLOW.md` - **Complete workflow guide (START HERE)**

## Quick Start

### 1. Review What Needs Translation

```bash
# View summary
head -50 french_comments_review.txt

# Or search for specific areas
grep "materials" french_comments_review.txt
```

### 2. Choose Your Approach

**Option A: AI Translation** (2-4 hours total)
```bash
export ANTHROPIC_API_KEY="your-key"
python3 batch_translate_ai.py --input french_comments_report.json --provider anthropic
```

**Option B: Manual Translation** (10-15 hours)
```bash
# Use french_comments_review.txt as your guide
nano french_comments_review.txt
```

**Option C: Team Translation** (3-5 hours divided)
```bash
python3 comment_translator.py template
# Distribute the template to team members
```

### 3. Apply Translations

```bash
python3 comment_translator.py apply --input your_translations.json
```

## Read the Full Guide

See `TRANSLATION_WORKFLOW.md` for:
- Detailed step-by-step instructions
- Cost estimates
- Quality assurance procedures
- Common French technical terms
- Prioritization strategy
- Troubleshooting

## Examples

### Find Comments in Material Models
```bash
cat french_comments_report.json | jq '.files[] | select(.file | contains("materials"))'
```

### Translate Specific Directory Only
```bash
# Extract materials only
cat french_comments_report.json | \
    jq '{files: [.files[] | select(.file | contains("materials"))]}' \
    > materials_only.json

# Translate just these
python3 batch_translate_ai.py --input materials_only.json --provider openai
```

### Check Progress
```bash
# Count remaining French comments
python3 comment_translator.py scan
```

## Need Help?

1. Read `TRANSLATION_WORKFLOW.md`
2. Check script help: `python3 comment_translator.py --help`
3. Review script source code for details

---

**Bottom line:** Only 348 comments need translation across 275 files. This is very manageable!
