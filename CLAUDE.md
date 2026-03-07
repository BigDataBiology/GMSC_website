# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Website for the Global Microbial smORFs Catalogue (GMSC) — a database of ~965 million small open reading frames from microbial metagenomes and genomes. Live at https://gmsc.big-data-biology.org/

## Build & Deploy

This is an Elm 0.19.1 single-page application deployed on Netlify.

```bash
# Local build (using build.sh)
./build.sh

# Or manually:
elm make --optimize src/Main.elm --output=dist/index.html
cp src/style.css dist/
cp -r src/assets dist/
cp _redirects dist/
```

There are no tests configured.

## Architecture

**Elm SPA with TEA (The Elm Architecture)** — each page is a module with its own Model/Msg/update/viewModel, wired together in `Main.elm`.

### Entry Point & Routing
- `src/Main.elm` — Top-level app. Defines the `Page` union type, routes messages to page modules, renders header/footer. Handles cross-page interactions (e.g., Home form submissions navigate to Sequence/Cluster/Mapper pages).
- `src/Route.elm` — URL parsing and route definitions.

### Page Modules (each follows Model/Msg/update/viewModel pattern)
- `Home.elm` — Landing page with search forms (identifier lookup, sequence search via GMSC-mapper, search ID lookup).
- `Sequence.elm` — Displays a single 100AA smORF (fetched from API).
- `Cluster.elm` — Displays a 90AA cluster with quality details; can show 100AA members via `Members.elm`.
- `Mapper.elm` — Sequence search: submits to API, polls for results (5s interval), displays annotations and hits. Supports result download.
- `Browse.elm` — Browse by habitat/taxonomy with quality filters. Uses `Filter.elm` for API queries and paginated results, and `Selectshared.elm`/`Selects.elm` for multi-select dropdowns.
- `Download.elm`, `Help.elm`, `About.elm` — Static content pages (Markdown rendered via `elm-explorations/markdown`).

### Supporting Modules
- `Filter.elm` — Handles browse/filter API calls, paginated result display (100 items/page).
- `Members.elm` — Fetches and displays 100AA members of a 90AA cluster.
- `Selectshared.elm`, `Selects.elm`, `Selectitem.elm` — Custom multi-select dropdown components for habitat/taxonomy/quality filters.
- `src/Select/` — Vendored/customized select widget (from `elm-select` package).
- `Shared.elm` — Fuzzy search filter utility.
- `View.elm` — Simple `View` type alias.

### Backend API
All data comes from `https://gmsc-api.big-data-biology.org/`. Key endpoints:
- `/v1/seq-info/{id}` — Single sequence/cluster info
- `/v1/seq-filter/` — Browse with filters (POST)
- `/v1/seq-info-multi/` — Batch sequence info (POST, JSON body)
- `/internal/seq-search/` — GMSC-mapper sequence search (POST, multipart)
- `/internal/seq-search/{id}` — Poll search results (GET)

### Static Assets
- `src/style.css` — Custom styles
- `src/assets/` — Images used in help and home pages

## Commit Message Convention

Commits use a prefix tag: `ENH` (enhancement), `BUG` (fix), `MIN` (minor/cosmetic), `RFCT` (refactor).

## Key Identifiers

- `GMSC10.100AA.XXX_XXX_XXX` — 100AA smORF identifiers (max: 964,970,495)
- `GMSC10.90AA.XXX_XXX_XXX` — 90AA cluster identifiers (max: 287,926,874)
