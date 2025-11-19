module About exposing (viewModel)
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

-- main text

content: Html msg
content = 
    span [id "introduction"]
        [Markdown.toHtml [] """
## The global microbial smORF catalogue (GMSC)

For more information see [(Duan et al., 2024)](https://www.nature.com/articles/s41467-024-51894-6).

GMSC was developed by the [Big Data Biology Research Group](https://www.big-data-biology.org/).
___
**Version:** &emsp;&ensp; 1.0
___
**Contacts:** &emsp; [yiqian@big-data-biology.org](yiqian@big-data-biology.org) and [luispedro@big-data-biology.org](luispedro@big-data-biology.org)
___
**Affiliations:** &ensp; Institute of Science and Technology for Brain-Inspired Intelligence ([ISTBI](https://istbi.fudan.edu.cn/)). Fudan University, Shanghai, China and [Centre for Microbiome Research](https://research.qut.edu.au/cmr/), School of Biomedical Sciences, Queensland University of Technology, Translational Research Institute, Woolloongabba, Queensland, Australia.
""" ]
