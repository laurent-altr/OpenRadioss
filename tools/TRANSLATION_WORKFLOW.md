# French Comment Translation Workflow for OpenRadioss

## Overview

This document describes an efficient workflow for translating French comments to English in the OpenRadioss Fortran codebase.

**Current Status (as of scan):**
- **Total Fortran files:** 5,128
- **Files with French comments:** 275 (5.4%)
- **Total French comments:** 348

This is a **manageable scope** that can be systematically addressed without manual editing of millions of lines.

---

## Quick Start

### Step 1: Review the Initial Report

The scan has already been completed. Review the findings:

```bash
# View human-readable summary
less tools/french_comments_review.txt

# Or view the JSON report for programmatic access
cat tools/french_comments_report.json | jq '.summary'
```

### Step 2: Choose Your Translation Approach

You have three options:

#### **Option A: AI-Assisted Translation (Recommended)**
Use AI to generate initial translations, then review and refine.

#### **Option B: Manual Translation**
Directly edit comments using the review file as a guide.

#### **Option C: Collaborative Batch Translation**
Create translation templates for team members to fill in.

---

## Option A: AI-Assisted Translation (Recommended)

This is the most efficient approach for 348 comments.

### Prerequisites

```bash
# Install required Python packages
pip install openai anthropic
```

### Using OpenAI GPT-4

```bash
# Set your API key
export OPENAI_API_KEY="your-api-key-here"

# Generate translations (processes all 275 files)
python3 tools/batch_translate_ai.py \
    --input tools/french_comments_report.json \
    --output tools/translations_ai.json \
    --provider openai \
    --batch-size 10

# Review and edit the translations
nano tools/translations_ai.json

# Apply the translations
python3 tools/comment_translator.py apply --input tools/translations_ai.json
```

### Using Anthropic Claude

```bash
# Set your API key
export ANTHROPIC_API_KEY="your-api-key-here"

# Generate translations
python3 tools/batch_translate_ai.py \
    --input tools/french_comments_report.json \
    --output tools/translations_ai.json \
    --provider anthropic \
    --batch-size 10

# Review and apply as above
```

### Cost Estimation

- **OpenAI GPT-4:** ~348 comments × 10 tokens avg = ~3,500 tokens ≈ $0.10-0.20
- **Anthropic Claude:** Similar cost structure
- **Processing time:** ~5-10 minutes for all files

### Review Process

After AI translation, **always review the output**:

1. Check technical terminology is accurate
2. Ensure context is preserved
3. Verify formatting matches original
4. Spot-check critical sections (material models, physics calculations)

---

## Option B: Manual Translation

For direct control or if AI is not available:

### Step 1: Use the Review File

```bash
# Open the review file in your editor
nano tools/french_comments_review.txt
```

### Step 2: Edit Files Directly

The review file shows:
- File path
- Line numbers
- French comment text

Edit each file manually using your preferred editor.

### Step 3: Track Progress

Create a checklist:

```bash
# Generate a simple checklist
grep "^File:" tools/french_comments_review.txt > translation_checklist.txt
```

Mark files as done:
```
[ ] engine/source/ale/alefvm/alefvm_stress.F
[x] engine/source/ams/sms_fsa_inv.F
[ ] engine/source/boundary_conditions/ebcs/ebcs6_inip.F
...
```

---

## Option C: Collaborative Batch Translation

For team-based translation:

### Step 1: Create Translation Template

```bash
python3 tools/comment_translator.py template \
    --input tools/french_comments_report.json \
    --output tools/translations_template.json
```

### Step 2: Distribute Work

Split the template file by directory or assign sections to team members:

```bash
# Example: Extract files from specific directory
cat tools/translations_template.json | \
    jq '.files[] | select(.file | startswith("engine/source/materials"))' \
    > translations_materials.json
```

### Step 3: Team Members Fill In Translations

Edit the JSON file and fill in the `"translation"` fields:

```json
{
  "line": 115,
  "original": "ALEMAIN>SFORC3() pour chaque groupe",
  "translation": "ALEMAIN>SFORC3() for each group",
  "full_line": "c     ALEMAIN>SFORC3() pour chaque groupe"
}
```

### Step 4: Merge and Apply

```bash
# Merge individual files back together
# ... (custom merging based on your team structure)

# Apply translations
python3 tools/comment_translator.py apply --input tools/translations_merged.json
```

---

## Advanced Workflows

### Prioritize Critical Files

Focus on high-impact areas first:

```bash
# Extract only material model files
cat tools/french_comments_report.json | \
    jq '.files[] | select(.file | contains("materials"))' \
    > priority_materials.json

# Translate these first
python3 tools/batch_translate_ai.py \
    --input priority_materials.json \
    --output translations_materials.json \
    --provider openai
```

### Progressive Translation

Translate in batches to manage costs and review burden:

```bash
# Translate first 50 files only
python3 tools/batch_translate_ai.py \
    --input tools/french_comments_report.json \
    --output translations_batch1.json \
    --provider openai \
    --limit 50

# Review and apply batch 1
python3 tools/comment_translator.py apply --input translations_batch1.json

# Then do batch 2, etc.
```

### HTML Review Interface

Generate a visual HTML file for easier review:

```bash
python3 tools/batch_translate_ai.py \
    --input tools/french_comments_report.json \
    --html

# Open review.html in your browser
firefox review.html
```

---

## Quality Assurance

### Before Applying Translations

1. **Backup your repository**
   ```bash
   git commit -am "Backup before translation"
   ```

2. **Test on a subset first**
   ```bash
   # Apply to one file manually, then test compilation
   ```

3. **Review technical terms**
   - "calcul" → "calculation"
   - "matrice" → "matrix"
   - "repere" → "reference frame"
   - "pression" → "pressure"
   - "contrainte" → "stress"
   - "deformation" → "strain"

### After Applying Translations

1. **Verify no code changes**
   ```bash
   git diff --check
   ```

2. **Ensure only comments were modified**
   ```bash
   git diff | grep "^-c" | head -20
   git diff | grep "^+c" | head -20
   ```

3. **Test compilation**
   ```bash
   # Your build command here
   make clean && make
   ```

---

## Common French Technical Terms

| French | English | Context |
|--------|---------|---------|
| calcul | calculation | "calcul de la matrice" → "calculation of the matrix" |
| avec | with | "avec impedance" → "with impedance" |
| pour | for | "pour chaque groupe" → "for each group" |
| dans | in | "dans le repere" → "in the reference frame" |
| sans | without | "sans packet" → "without packet" |
| pression | pressure | "pression initiale" → "initial pressure" |
| repere | reference frame | "repere global" → "global reference frame" |
| armature | reinforcement | Material science context |
| beton | concrete | Material science context |
| vitesse | velocity | "vitesse initiale" → "initial velocity" |
| deformation | strain | Mechanics context |
| contrainte | stress | Mechanics context |
| effort | force | "effort nodal" → "nodal force" |
| noeud | node | "noeud element" → "element node" |
| masse | mass | "matrice de masse" → "mass matrix" |
| suivant | following/along | Context dependent |
| initial/initiale | initial | "valeur initiale" → "initial value" |
| matrice | matrix | "matrice d'inertie" → "inertia matrix" |
| vecteur | vector | Linear algebra context |

---

## Directory Prioritization

Based on the scan, French comments are distributed across these key areas:

1. **Materials (`engine/source/materials/`)** - 40+ files
   - Material models (LAW24, LAW63, etc.)
   - Constitutive relations
   - Physics implementations

2. **Boundary Conditions (`engine/source/boundary_conditions/`)** - 15+ files
   - Pressure boundary conditions
   - Impedance calculations

3. **Constraints (`engine/source/constraints/`)** - 20+ files
   - Rigid body dynamics
   - Inertia calculations

4. **Elements (`engine/source/elements/`)** - 80+ files
   - Shell elements
   - Solid elements
   - Beam elements

5. **Contact/Interfaces (`engine/source/interfaces/`)** - 30+ files

6. **Starter modules (`starter/source/`)** - 50+ files

**Recommended order:**
1. Start with materials (highest technical value)
2. Boundary conditions and constraints
3. Element implementations
4. Contact/interface modules
5. Utility and starter modules

---

## Troubleshooting

### Script Issues

**Problem:** Script can't find files
```bash
# Run from repository root
cd /home/user/OpenRadioss
python3 tools/comment_translator.py scan
```

**Problem:** Encoding errors
The scripts use `latin-1` encoding which is standard for Fortran. If you see errors, check file encoding:
```bash
file -i path/to/file.F
```

### Translation Quality

**Problem:** AI translation is too literal
- Review and adjust technical terms manually
- Provide context in prompts for specific domains

**Problem:** Formatting broken after applying
- Check that only comment lines were modified
- Verify indentation is preserved

### Git Issues

**Problem:** Large diffs
- This is expected when modifying 275 files
- Consider committing in batches by directory

```bash
# Commit by module
git add engine/source/materials/
git commit -m "Translate French comments in material models"
```

---

## Example Complete Workflow

Here's a complete example using AI translation:

```bash
# 1. Navigate to repository
cd /home/user/OpenRadioss

# 2. Review existing scan
head -50 tools/french_comments_review.txt

# 3. Set up AI translation (using Anthropic Claude)
export ANTHROPIC_API_KEY="your-key"

# 4. Translate materials first (priority)
cat tools/french_comments_report.json | \
    jq '{files: [.files[] | select(.file | contains("materials"))]}' | \
    jq '. + {summary: {files_with_french: (.files | length)}}' \
    > tools/materials_only.json

python3 tools/batch_translate_ai.py \
    --input tools/materials_only.json \
    --output tools/translations_materials.json \
    --provider anthropic

# 5. Review translations
nano tools/translations_materials.json

# 6. Create backup
git commit -am "Backup before translating material comments"

# 7. Apply translations
python3 tools/comment_translator.py apply --input tools/translations_materials.json

# 8. Review changes
git diff engine/source/materials/ | less

# 9. Test compilation
make clean && make

# 10. Commit if successful
git add engine/source/materials/
git commit -m "Translate French comments in material models to English"

# 11. Repeat for other directories
```

---

## Files Created

After running the initial scan, these files are available:

- `tools/french_comments_report.json` - Machine-readable report (275 files)
- `tools/french_comments_review.txt` - Human-readable review file (2,460 lines)
- `tools/comment_translator.py` - Main translation tool
- `tools/batch_translate_ai.py` - AI-assisted translation script
- `tools/TRANSLATION_WORKFLOW.md` - This document

---

## Next Steps

1. **Decide on approach** (AI, manual, or collaborative)
2. **Set priority** (which directories first)
3. **Execute translation** (using appropriate workflow above)
4. **Review and test** (verify compilation and correctness)
5. **Commit changes** (in logical batches)

For questions or issues, refer to the script source code or reach out to the team.

---

## Summary Statistics

- **Scope:** 348 comments in 275 files out of 5,128 total files (5.4%)
- **Effort Estimate:**
  - AI-assisted: 2-4 hours (including review)
  - Manual: 10-15 hours
  - Collaborative: 3-5 hours (divided among team)
- **Cost Estimate:** $0.10-0.50 for AI services
- **Impact:** Improved code maintainability and accessibility for international contributors

This is a **very manageable** translation project that will significantly improve code quality!
