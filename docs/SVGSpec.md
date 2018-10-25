# SVG Spec Documentation
Specification for SVG files for use as game assets.

## Overview
SVG asset files must expose some expected IDs. How to use the asset files is out of scope of this document.
IDs defined are case sensitive.

## Tile and Unit Sizes
Sizes may be any dimensions, but must be square, and must be consistent throughout a single svg.

## External resources
SVGs must not rely on external resources. They must include all resources internally.

## Expected Tile IDs
The following IDs must be exposed for tiles.
```
- tile_brick
- tile_concrete
```

## Expected Unit IDs
The following IDs must be exposed for units.
```
- unit_swords
- unit_magic
- unit_guns
```
