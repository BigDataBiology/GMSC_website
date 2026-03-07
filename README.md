# GMSC Website

Website for the **Global Microbial smORFs Catalogue (GMSC)** — an integrated, consistently-processed catalogue of ~965 million small open reading frames (smORFs) from microbial metagenomes and high-quality isolated genomes.

Live at: https://gmsc.big-data-biology.org/

## Citation

> Duan, Y., Qi, C., Lawley, T.D. *et al.* A catalogue of small proteins from the global microbiome. *Nat Commun* **15**, 7394 (2024). https://doi.org/10.1038/s41467-024-51894-6

## Building

This is an [Elm](https://elm-lang.org/) 0.19.1 single-page application.

```bash
./build.sh
```

This compiles `src/Main.elm` into `dist/index.html` and copies static assets (`style.css`, `assets/`, `_redirects`) into `dist/`.

## Deployment

Deployed on [Netlify](https://www.netlify.com/). See `netlify.toml` for configuration.

## License

See [LICENSE](LICENSE).
