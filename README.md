# Chinese urban accessibility policy corpus 2011–2026

Research materials for a policy-document study of accessibility verification responsibilities and user participation in China.

## Included materials

| Path | Contents |
|---|---|
| `S1_core/` | 67 dedicated accessibility policy document texts |
| `S2_context/` | 13 contextual policy document texts |
| `manifest.csv` | Register of 80 document versions, original titles, source URLs and analytical roles |
| `crosswalk_map.csv` | 80-row crosswalk between document identifiers and source titles |
| `双编码对比矩阵.csv` | Original paired coding assignments for 66 reviewed versions |
| `分歧裁决清单.csv` | 26 recorded coding discrepancies, copied as provided |
| `18张图.R` | Complete figure-generation R script, copied as provided |
| `generate_all_figures.R` | Exact-byte ASCII-named alias of `18张图.R` |
| `data/`, `generators/`, `source_evidence/` | Required numeric, geometry and evidence inputs from the checked figure package |
| `utf8_csv/` | UTF-8 viewing copies of originally GB18030-encoded CSVs; original root files are preserved |
| `upload_manifest_sha256.csv` | Sizes and SHA-256 checksums for the 85 files in this repository |

The 80 registered versions are a bounded study corpus, not an exhaustive inventory of Chinese accessibility policy. The figure analysis uses 59 urban-eligible versions and a 56-version strict sensitivity set. The paired-review matrix is a separate review set and does not redefine those denominators.

## Reproduce the figures

Use an R environment with Cairo support and Times New Roman installed. The scripts use base R; no additional R packages are required. Validation used Windows R 4.6.1.

From the repository root, run:

```sh
Rscript generate_all_figures.R
```

The script recreates 18 SVGs in `svg/`, including the optional circular clustered heatmap, and writes checks and intermediate tables. These generated files are ignored by Git. Keep the three input directories beside the R entry point.

## Source and analysis snapshots

The two source folders and four root CSVs preserve the files supplied for this upload byte for byte. The top-level paired matrix and discrepancy file are source snapshots. The figure script reads its checked analysis inputs from `source_evidence/` and `generators/`, including the recorded decisions and the documented source checks; these files must not be substituted merely because another CSV has a similar name. All source checks were verified by the authors against the original policy documents. Original C1/C2 assignments and pre-adjudication agreement remain unchanged.

The five-dimension and actor/basis ordinal summaries remain exploratory; the binary intercoder agreement assessment does not validate those separate scoring layers. The original register labels are preserved; the analysis applies its documented issuing-level harmonisation rather than silently editing the source register.

## Encoding and provenance

Policy texts and code retain their supplied contents. Root CSV encoding is recorded in `metadata/csv_encoding.json`; `utf8_csv/` provides readable equivalents where conversion is needed. UTF-8 conversions change encoding only, not rows or values. Public policy source URLs are listed in the register. Policy documents remain attributable to their issuing bodies; inclusion does not imply authorship of those documents.

## 中文说明

本仓库原样保存67份核心政策文本、13份背景政策文本、四个CSV及完整R代码。为确保R可运行，同时附上此前核对的数值、地图几何和证据输入。原始文件不覆写；GB18030编码的CSV另提供UTF-8查看副本。完整R会重现18张SVG，研究样本及评分边界见上文。
