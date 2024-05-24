module Help exposing (viewModel)
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

content : Html msg
content =
    span [id "help"]
        [Markdown.toHtml [] """
## Overview
The global microbial smORFs catalogue (GMSC) is an integrated, consistently-processed, smORFs catalogue of the microbial world, combining 63,410 publicly available metagenomes from the [SPIRE database](http://spire.embl.de) and 87,920 high-quality isolated microbial genomes from the [ProGenomes2 database](https://progenomes.embl.de/).

A total of 4.5 billion smORFs were predicted to build the catalogue. After removing redundancy with 100% amino acid itentity, we obtained a 100AA non-redundant catalogue with 964,970,496 sequences. Further, the smORFs were clustered at 90% amino acid identity resulting in 287,926,875 90AA smORFs catalogue.

## Benefits and Features
**Integration:**

- GMSC is available as a web resource that displays each smORFs for browsing their integrated annotations:
  - clusters
  - taxonomy classification
  - habitat assignment
  - quality assessment
  - conserved domain annotation
  - cellular localization prediction

**Main purpose of GMSC**:
- Expand smORFs sets of global microbiomes with comprehensive annotation
- Analyse of ecological distribution patterns across taxonomy and global habitats
- Annotate smORFs of microbial genomes or genes with the resource

## Searching

##### Search from identifier
smORFs in the catalogue are identified with the scheme `GMSC10.100AA.XXX_XXX_XXX` or `GMSC10.90AA.XXX_XXX_XXX`. The initial `GMSC10` indicates the version of the catalogue (Global Microbial smORFs Catalogue 1.0). The `100AA` or `90AA` indicates the amino acid identity of the catalogue. The `XXX_XXX_XXX` is a unique numerical identifier (starting at zero). Numbers were assigned in order of increasing number of copies. So that the greater the number, the greater number of copies of that peptide were present in the raw data. 

On the 100AA Sequence page, the below information will be displayed for each sequence.

- Protein sequence
- Nucleotide sequence
- Taxonomic assignment
- Habitat
- Protein cluster
- Quality

On the 90AA Cluster page, the below information will be displayed for each cluster. And the 100AA members of the cluster will be displayed by pressing the `show` button. 

- Consensus protein sequence
- Consensus nucleotide sequence
- Taxonomic assignment
- Habitat
- Number of 100AA smORFs
- Quality

##### Find homologues by sequence (GMSC-mapper)

GMSC-mapper is provided as a search tool to query sequences. Users can provide contigs or protein sequences and it will return a set of smORFs with complete annotations that match the 90AA smORFs in GMSC.

The searching will take ~15 min. A search id will be provided for each query task. Search ID are of the form `#-xxxx` where `#` is just an index counting up and `xxxx` is a random string. 

Users can wait results at the Mapper page or look up results by the search id from the Home page later.

GMSC-mapper can also be downloaded and run locally, please see details in [Github page](https://github.com/BigDataBiology/GMSC-mapper)

![GMSC-mapper](assets/help_tool.png)

## Browse
Users can browse by habitats and taxonomy. (e.g., passing marine will match freshwater,marine,human gut). Multiple habitats can be selected.

The results are 90AA smORFs families span in selected habitats and taxonomy.


## Data acquistion
63,410 assembled metagenomes were used from the [SPIRE database](http://spire.embl.de)

87,920 high-quality microbial genomes were downloaded from the [ProGenomes v2 database](https://progenomes.embl.de/).

## Construction of GMSC
![GMSC-mapper](assets/help_overview.png)
##### Prediction of smORFs
- We used a modified version of [Prodigal](https://github.com/hyattpd/Prodigal) to predict ORFs &gt;= 15 bps.  The ORFs that &lt;= 300 bps were considered smORFs.

##### Cluster generation
All predicted smORFs were removed redundancy with 100% amino acid identity. Then they were clustered with 90% amino acid identity and 90% coverage using [Linclust](https://github.com/soedinglab/MMseqs2).

##### Taxonomy & Habitat annotation
- ** Taxonomy annotation:**
  - The taxonomy of assembled contigs encoding the small proteins was annotated using [MMSeqs2](https://github.com/soedinglab/MMseqs2) against the [GTDB database](https://gtdb.ecogenomic.org/). 
  - We recorded the taxonomy of smORFs based on the taxonomy of the contigs of metagenomes or genomes of [Progenome v2 database](https://progenomes.embl.de/) from which the smORFs were predicted. Subsequently, we assign taxonomy for 100AA and 90AA smORFs using the lowest common ancestor (LCA), ignoring the un-assigned rank to make it more specific.
- **Habitat annotation:**
  - We recorded the habitats of smORFs according to their source samples using the habitat microontology introduced in the [SPIRE database](http://spire.embl.de). We further grouped the habitats into 8 broad categories: mammal gut, anthropogenic, other-human, other-animal, aquatic, human gut, soil/plant, and other.

##### Conserved domain annotation
The representative sequences of 90AA smORFs families were searched against [NCBI CDD database](https://www.ncbi.nlm.nih.gov/cdd/) by RPS-blast. Hits with an e-value maximum of 0.01 and at least 80% of coverage of PSSM's length were retained and considered significant.

##### Cellular localization prediction
- [TMHMM2](https://services.healthtech.dtu.dk/services/TMHMM-2.0/) was run on the representative sequences of 90AA smORFs families. 
- [SignalP-5.0](https://services.healthtech.dtu.dk/services/SignalP-5.0/) was run on the representative sequences of 90AA smORFs families both with '-org gram+' and '-org gram-' , and '-org arch' modes.

##### Quality assessment
- **Terminal check:** We checked for the presence of an in-frame STOP upstream of smORFs. As a smORF predicted at the start of a contig that is not preceded by an in-frame STOP codon risks being a false positive originating from an interrupted fragment.
- **Antifam:** We used HMMSearch to search smORFs against the [Antifam database 7.0](https://github.com/ebi-pf-team/antifam) to avoid spurious smORFs
- **RNAcode:** We used [RNAcode](https://github.com/ViennaRNA/RNAcode), a tool to predict the coding potential of sequences based on evolutionary signatures, to identify the coding potential of 90AA smORF families containing > 8 sequences. 
- **Metatranscriptomes:** We downloaded 221 sets of publicly available metatranscriptome data from the NCBI database paired with the metagenomic samples we used in our catalogue. We mapped reads against the representative smORFs of 90AA families by [BWA](https://github.com/lh3/bwa). The smORFs are considered to have transcriptional evidence only if they are mapped by reads across at least 2 samples. 
- **Ribo-Seq:** We downloaded 142 publicly available Ribo-Seq sets from the NCBI database.  We mapped reads against the representative smORFs of 90AA families by [BWA](https://github.com/lh3/bwa). The smORFs are considered to have translation evidence only if they are mapped by reads across at least 2 samples. 
- **Metaproteomes:** We downloaded peptide datasets from 108 metaproteome projects of the [PRIDE database](https://www.ebi.ac.uk/pride/). We exactly matched 100AA smORFs to the identified peptides of each project. If the total k-mer coverage of peptides on a smORF is greater than 50%, then the smORF is considered translated and detected.
""" ]