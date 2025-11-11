# Score Validation Results

## Summary

**Status**: ✗ VALIDATION FAILED

Out of 404 schools validated, issues were found in **~15 schools** across multiple competitions.

## Important Rule

**Fields ending in "_Tot"** should be in the 0-20 range.
**Music_Total and Visual_Total** should be in the 0-20 range.
**Sub_Total, Weighted_Total, and Final_Total** can exceed 20 (they are aggregate scores).

## Test Coverage

### Fields Ending in "_Tot" (Expected: 0-20)
- ✓ **Music_Ind_Tot**: OK (8.7 - 18.4)
- ✓ **Music_Ens_Tot**: OK (8.6 - 18.3)
- ✓ **Visual_Ens_Tot**: OK (8.6 - 18.8)
- ✗ **Visual_Ind_Tot**: 4 schools out of range (8.9 - 56.5)
- ✗ **Music_Eff1_Tot**: 11 schools out of range (8.8 - 64)
- ✗ **Music_Eff2_Tot**: 11 schools out of range (8.1 - 60)
- ✗ **Visual_Eff_Tot**: 11 schools out of range (8.3 - 35.85)

### Component "_Total" Fields (Expected: 0-20)
- ✗ **Music_Total**: 8.7 - 54 (4 schools out of range)
- ✗ **Visual_Total**: 8.85 - 59 (11 schools out of range)

### Aggregate "_Total" Fields (Can exceed 20)
- ✓ **Sub_Total**: 25.2 - 58.6 (aggregate - expected behavior)
- ✓ **Weighted_Total**: 0 - 90.9 (aggregate - expected behavior)
- ✓ **Final_Total**: 15 - 90.9 (aggregate - expected behavior)

### Penalty Scores
- ✓ **Timing_Penalty**: Can be any value (typically small negative or positive)
- ✗ **Timing_Penalty_Tot**: Expected -10 to 0, but some schools have values outside this range

## Schools with Data Issues

### Critical Issues (Values 29-64 range in "_Tot" fields)
These schools have values in "_Tot" fields suggesting raw judge scores were extracted instead of calculated totals:

1. **Adams City High School** (CBA 2A-4A Metro Regional 2024)
   - Visual_Ind_Tot: 52 ✗
   - Music_Eff1_Tot: 54 ✗
   - Visual_Eff_Tot: 31.5 ✗

2. **Monte Vista HS** (Legend Invitational)
   - Visual_Ind_Tot: 56.5 ✗
   - Music_Eff1_Tot: 58.5 ✗
   - Music_Eff2_Tot: 52 ✗
   - Visual_Eff_Tot: 34.4 ✗

3. **Buena Vista HS** (Legend Invitational)
   - Music_Eff1_Tot: 52 ✗
   - Music_Eff2_Tot: 54 ✗
   - Visual_Eff_Tot: 32.8 ✗

4. **Elizabeth HS** (Legend Invitational)
   - Music_Eff1_Tot: 64 ✗
   - Music_Eff2_Tot: 60 ✗
   - Visual_Eff_Tot: 35.85 ✗

5. **Grand Valley HS** (Colorado West)
   - Visual_Ind_Tot: 51 ✗

6. **Pueblo Central** (Multiple competitions)
   - Music_Eff1_Tot: 50-53 ✗
   - Music_Eff2_Tot: 50-52 ✗
   - Visual_Eff_Tot: 30.8 ✗

7. **Sterling HS** (CBA North Regional 2024)
   - Music_Eff1_Tot: 50 ✗
   - Music_Eff2_Tot: 48 ✗
   - Visual_Eff_Tot: 29.9 ✗

## Root Cause Analysis

### Likely Causes:
1. **PDF Format Differences**: Some schools may have different scoresheet layouts
2. **Ranking Number Filtering**: The filter for ranking numbers (1-10) may not be catching all cases
3. **Raw Judge Scores**: Individual judge scores (typically 45-80) being captured instead of averaged totals (0-20)
4. **Missing Totals Column**: Some PDFs may not have clear total columns, causing extraction of wrong values

### Affected Competitions:
- Legend Invitational (multiple schools)
- CBA 2A-4A Metro Regional 2024
- Colorado West
- CBA North Regional 2024
- Print Back to Top
- Harrison Marching Invitational

## Recommendations

1. **Manual Review**: Examine PDF files for the problem schools to understand formatting differences
2. **Enhanced Filtering**: Improve the numeric filtering logic in `read_file.R` to:
   - Better distinguish between raw judge scores (45-80) and totals (0-20)
   - Handle different PDF layouts
3. **Data Flagging**: Add a validation flag column to mark suspicious scores
4. **Alternative Extraction**: Consider using different extraction logic for problem PDFs

## How to Run Tests

```r
# Basic test - all _Tot variables
source('R/test_tot_variables.R')

# Refined test - categorized by score type
source('R/test_score_validation.R')
```

## Impact

While ~15 schools have data quality issues in specific "_Tot" fields, the remaining **~389 schools (96%)** have valid data within expected ranges. The dataset is still usable for most analyses, but results for the affected schools should be interpreted with caution.

**Validation Rules**:
- Fields ending in "_Tot": must be 0-20
- Music_Total, Visual_Total: must be 0-20 (but ~11-15 schools have extraction issues)
- Sub_Total, Weighted_Total, Final_Total: can exceed 20 (they are aggregate scores)
- Timing_Penalty_Tot: must be -10 to 0
