module Download exposing (viewModel)
import Html exposing (..)
import Html.Attributes as HtmlAttr
import Html.Attributes exposing (..)
import Browser
import Dict
import Markdown
import View exposing (View)

import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row

viewModel : Html msg
viewModel =
    content

content: Html msg
content = 
    span [id "introduction"]
        [Markdown.toHtml [] """
# Data Downloads
We provide a 100% non-redundant catalog (964,970,496 smORFs) and a 90% amino-acid level catalog (287,926,875 smORFs families).

- The download files contain: 
  - protein / nucleotide fasta file
  - cluster table
  - taxonomy classification
  - habitat assignment
  - quality assessment
  - conserved domains annotation
  - cellular localization prediction
  - metadata

### Protein sequence (.fasta)
Fasta file of 100AA / 90AA protein sequences.

**100AA catalog:**&emsp;[GMSC10.100AA.faa.xz](https://gmsc-api.big-data-biology.org/files/GMSC10.100AA.faa.xz)

**90AA catalog:**&emsp;[GMSC10.90AA.faa.xz](https://gmsc-api.big-data-biology.org/files/GMSC10.90AA.faa.xz)

### Nucleotide sequence (.fasta)
Fasta file of 100AA / 90AA nucleotide sequences.

**100AA catalog:**&emsp;[GMSC10.100AA.fna.xz](https://gmsc-api.big-data-biology.org/files/GMSC10.100AA.fna.xz)

**90AA catalog:**&emsp;[GMSC10.90AA.fna.xz](https://gmsc-api.big-data-biology.org/files/GMSC10.90AA.fna.xz)

### Clusters
TSV table relating 100AA smORF accession and the hierarchically obtained clusters at 90% amino acid identity (which represent sequences with the same function).

Columns:

- `100AA smORF accession`
- `90AA smORF accession`

**Protein clustering table:**&emsp;[GMSC10.cluster.tsv.xz](https://gmsc-api.big-data-biology.org/files/GMSC10.cluster.tsv.xz)

### Taxonomy classification
TSV table relating the taxonomy classification of 100AA / 90AA catalog.

Columns:

- `smORF accession`
- `taxonomy (separated by semicolon)`

**100AA catalog:**&emsp;[GMSC10.100AA.taxonomy.tsv.xz](https://gmsc-api.big-data-biology.org/files/GMSC10.100AA.taxonomy.tsv.xz)

**90AA catalog:**&emsp;[GMSC10.90AA.taxonomy.tsv.xz](https://gmsc-api.big-data-biology.org/files/GMSC10.90AA.taxonomy.tsv.xz)

### Habitat assignment
TSV table relating the habitat assignment of 100AA / 90AA catalog.

Columns:

- `smORF accession`
- `habitats (separated by comma)`


**100AA catalog:**&emsp;[GMSC10.100AA.general_habitat.tsv.xz](https://gmsc-api.big-data-biology.org/files/GMSC10.100AA.general_habitat.tsv.xz)

**90AA catalog:**&emsp;[GMSC10.90AA.general_habitat.tsv.xz](https://gmsc-api.big-data-biology.org/files/GMSC10.90AA.general_habitat.tsv.xz)

### Quality assessment
TSV table relating the quality assessment of 100AA / 90AA catalog.

Columns:

- `smORF accession`
- `RNAcode`
- `AntiFam`
- `metaProteome`
- `(meta)Riboseq`
- `Terminal checking`
- `metaTranscriptome`

**100AA catalog:**&emsp;[GMSC10.100AA.quality.tsv.xz](https://gmsc-api.big-data-biology.org/files/GMSC10.100AA.quality.tsv.xz)

**90AA catalog:**&emsp;[GMSC10.90AA.quality.tsv.xz](https://gmsc-api.big-data-biology.org/files/GMSC10.90AA.quality.tsv.xz)

### Conserved domains annotation
TSV table relating the Conserved domains annotation of 90AA catalog.

Columns:

- `smORF accession`
- `identifier in CDD database (separated by comma)`

**90AA catalog CDD annotation:**&emsp;[GMSC10.90AA.cdd.tsv.xz](https://gmsc-api.big-data-biology.org/files/GMSC10.90AA.cdd.tsv.xz)

### Cellular localization prediction
TSV table relating the TMHMM prediction of 90AA catalog.

Columns:

- `smORF accession`
- `The length of the protein sequence`
- `The expected number of amino acids intransmembrane`
- `The expected number of amino acids in transmembrane helices in the first 60 amino acids of the protein`
- `The number of predicted transmembrane helices by N-best`
- `The topology predicted by N-best`

**90AA catalog TMHMM prediction:**&emsp;[GMSC10.90AA.TMHMM.tsv.xz](https://gmsc-api.big-data-biology.org/files/GMSC10.90AA.TMHMM.tsv.xz)

TSV table relating the SignalP prediction of 90AA catalog.

Columns:

- `smORF accession`
- `Cmax`
- `pos`
- `Ymax`
- `pos`
- `Smax`
- `pos`
- `Smean`
- `D`
- `? signal pepetide`
- `Dmaxcut`
- `Networks-used`

**90AA catalog SignalP Gram+ prediction:**&emsp;[GMSC10.90AA.SignalP.gram+.tsv.xz](https://gmsc-api.big-data-biology.org/files/GMSC10.90AA.SignalP.gram+.tsv.xz)

**90AA catalog SignalP Gram- prediction:**&emsp;[GMSC10.90AA.SignalP.gram-.tsv.xz](https://gmsc-api.big-data-biology.org/files/GMSC10.90AA.SignalP.gram-.tsv.xz)

### Metadata
TSV table relating the metadata of GMSC.

Columns:

- `smORF accession`
- `samples (separated by comma)`

**100AA catalog original sample:**&emsp;[GMSC10.100AA.sample.tsv.xz](https://gmsc-api.big-data-biology.org/files/GMSC10.100AA.sample.tsv.xz)

**90AA catalog original sample:**&emsp;[GMSC10.90AA.sample.tsv.xz](https://gmsc-api.big-data-biology.org/files/GMSC10.90AA.sample.tsv.xz)

TSV table relating to metadata of samples used in GMSC.

Columns:

- `sample_accession`
- `ena_ers_sample_id`
- `database`
- `access_status`
- `study`
- `study_accession`
- `publications`
- `collection_date`
- `aliases`
- `microontology`
- `environment_biome`
- `environment_feature`
- `environment_material`
- `geographic_location`
- `latitude`
- `longitude`
- `tax_id`
- `tax_scientific_name`
- `host_common_name`
- `host_scientific_name`
- `host_tax_id`
- `general_envo_name`
- `higher_environment`
- `gender`
- `age_years`
- `subject_disease_status`
- `antibiotic`
- `notes`

**Metadata:**&emsp;[GMSC10.metadata.tsv](https://gmsc-api.big-data-biology.org/files/GMSC10.metadata.tsv)
"""]