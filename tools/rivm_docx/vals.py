"""Load benchmark outputs written by R/rivm_benchmark.qmd (never hand-type numbers)."""
import csv, os

PROJ = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", ".."))
OUT = os.path.join(PROJ, "data_output")
FIG = os.path.join(PROJ, "figures")


def _read(name):
    with open(os.path.join(OUT, name), encoding="utf-8") as f:
        return list(csv.DictReader(f))


VALS = {r["key"]: r for r in _read("rivm_benchmark_text_values_v1.csv")}
CAPS = {r["figure"]: r["caption"] for r in _read("rivm_benchmark_captions_v1.csv")}
SAMPLES = _read("rivm_benchmark_samples_v1.csv")
GROUPS = _read("rivm_benchmark_groups_v1.csv")
AUDIT = _read("rivm_benchmark_audit_v1.csv")
SITABLE = _read("rivm_benchmark_si_table_v1.csv")
PUBCHECK = _read("rivm_benchmark_published_vs_reconstructed_v1.csv")


def f(key):
    return VALS[key]["formatted"]


def num(key, col="value"):
    return float(VALS[key][col])


def headline(key):
    """Median only (text before the parenthesis)."""
    return f(key).split(" (")[0]


def group(gname, variant, window, scenario="primary"):
    for g in GROUPS:
        if g["group"] == gname and g["method_variant"] == variant and g["window"] == window and g["scenario"] == scenario:
            return g
    raise KeyError((gname, variant, window))


def sample_medians(gname, variant, window, scenario="primary"):
    return [float(s["conc_p50"]) for s in SAMPLES if s["group"] == gname and s["method_variant"] == variant
            and s["window"] == window and s["scenario"] == scenario]


def fix_citations(text, mapping):
    for k, v in mapping.items():
        text = text.replace(k, v)
    return text
