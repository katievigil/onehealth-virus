#!/bin/bash

# Exit on errors, undefined vars, and pipe fails
set -euo pipefail

# === Input file ===
INPUT="path/to/input"

# === Output prefix ===
PREFIX="fasta/file/name"

# === Output file names ===
CLEANED="${PREFIX}_cleaned.fasta"
ALIGN="${PREFIX}_aligned.fasta"
TRIM="${PREFIX}_trimmed.fasta"
OUTNAME="${PREFIX}_phylo"
TREE="${PREFIX}_tree.nwk"

# === Helper for timestamped logs ===
log() {
    echo -e "[$(date '+%Y-%m-%d %H:%M:%S')] $1"
}

# === Check input file ===
if [ ! -f "$INPUT" ]; then
    log "❌ Input file not found: $INPUT"
    exit 1
fi

# === Check required tools ===
for tool in seqkit mafft trimal iqtree FastTree; do
    if ! command -v $tool &> /dev/null; then
        log "❌ Required tool not found in PATH: $tool"
        exit 1
    fi
done

# === Step 0: Clean FASTA file ===
log "🧼 Cleaning FASTA file..."
# Remove non-nucleotide characters (r, y, s, w, k, m, b, d, h, v) and unwanted symbols
seqkit seq --validate-seq --remove-gaps --upper-case "$INPUT" > "$CLEANED"

# === Step 1: Align with MAFFT ===
log "📐 Aligning sequences with MAFFT..."
mafft --anysymbol "$CLEANED" > "$ALIGN"

# === Step 2: Trim poorly aligned regions ===
log "✂️ Trimming alignment with trimAl..."
trimal -in "$ALIGN" -out "$TRIM" -automated1

# === Step 3: Build tree ===
TREE_TOOL="${1:-fasttree}"
case "$TREE_TOOL" in
    iqtree)
        log "🌳 Building tree with IQ-TREE..."
        iqtree -s "$TRIM" -nt AUTO -pre "$OUTNAME"
        TREE="${OUTNAME}.treefile"
        ;;
    fasttree)
        log "🌳 Building tree with FastTree..."
        FastTree -nt "$TRIM" > "$TREE"
        ;;
    *)
        log "❌ Invalid option: '$TREE_TOOL'. Use 'fasttree' or 'iqtree'."
        exit 1
        ;;
esac

log "✅ Tree created: $TREE"
