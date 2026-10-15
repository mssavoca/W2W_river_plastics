"""mpsizebase_ocean_windows_v1.py (v1)
Window concentrations for the OCEAN PSDs of MPsizeBase (v19-5-2026 spreadsheet; sheets 'Literature data summary' + 'Literature PSD data').
For each PSD and window [lo,hi] (um):
  direct_  = sum of reported bin concentrations (MP/m3) overlapping the window, partial bins prorated linearly in size  [observation-based]
             valid only if the PSD's reported size range covers the window.
  fit_     = 10^b * (lo^a' - hi^a')   from MPsizeBase's fitted C-PSD line (a' = cumsum_slope, b = log intercept)  [model-based]
             'fit_in_LOD' = window lies inside the PSD's assigned LOD window (min filtered_low_size .. max filtered_high_size).
  MPsizeBase's own '1-5000 um extrapolated' equals 10^b*(1-5000^a') (verified numerically for all 99 ocean PSDs).
Differential slope a = a' - 1 (column cumsum_powerlaw_alpha); this is the same convention as our a_psd.
Usage: python3 mpsizebase_ocean_windows_v1.py <MPsizeBase.xlsx> <outdir>
"""
import sys, os, warnings, numpy as np, pandas as pd
warnings.filterwarnings("ignore")
f, out = sys.argv[1], sys.argv[2]
def load(sheet):
    d = pd.read_excel(f, sheet, header=None)
    names = [(str(n) if pd.notna(n) else f"col{i}") for i, n in enumerate(d.iloc[2])]
    X = d.iloc[3:].copy(); X.columns = names
    return X.dropna(how="all")
S = load("Literature data summary"); P = load("Literature PSD data")
S = S[S.Sample_type == "Ocean"].copy(); P = P[P.Sample_type == "Ocean"].copy()
num_s = ["Lat","Lon","Depth_inf","Depth_sup","Mesh_size","Volume_sampled","MP_Nconc_reported","cumsum_slope","cumsum_slope_SD","cumsum_log_intercept",
         "cumsum_powerlaw_alpha","reported_min_size","reported_max_size","cumsum_MP_1_5000_mean","cumsum_MP_1_5000_low","cumsum_MP_1_5000_high","PSD_MP_count","Sample_MP_count","f_filter_analyzed"]
for c in num_s: S[c] = pd.to_numeric(S[c], errors="coerce")
for c in ["Low_size","High_size","Bin_concentration","filtered_low_size","filtered_high_size"]: P[c] = pd.to_numeric(P[c], errors="coerce")
lod = P.groupby("PSD_ID").agg(lod_lo=("filtered_low_size","min"), lod_hi=("filtered_high_size","max"))
S = S.merge(lod, left_on="PSD_ID", right_index=True, how="left")
W = {"165_385": (165,385), "150_500": (150,500), "50_500": (50,500), "100_500": (100,500), "100_5000": (100,5000), "355_5000": (355,5000), "500_5000": (500,5000), "1_5000": (1,5000)}
bins = {k: g for k, g in P.dropna(subset=["Low_size","High_size","Bin_concentration"]).groupby("PSD_ID")}
def direct(psd, lo, hi):
    g = bins.get(psd)
    if g is None: return np.nan, np.nan
    ov = (np.minimum(g.High_size, hi) - np.maximum(g.Low_size, lo)).clip(lower=0)
    w = ov / (g.High_size - g.Low_size); part = ((ov > 0) & (ov < (g.High_size - g.Low_size) * 0.999))
    tot = (g.Bin_concentration * w).sum(); ptot = (g.Bin_concentration * w)[part].sum()
    return tot, (ptot / tot if tot > 0 else np.nan)
rows = []
for r in S.itertuples():
    base = dict(PSD_ID=r.PSD_ID, Study=str(r.Study).strip(", "), Year=r.Year, Location=r.Location, Lat=r.Lat, Lon=r.Lon, Sub_type1=r.Sub_type1, Sub_type2=r.Sub_type2,
                Depth_inf=r.Depth_inf, Depth_sup=r.Depth_sup, MP_shape=r.MP_shape, Sampling=r.Broad_sampling_method, Mesh_um=r.Mesh_size, Volume_m3=r.Volume_sampled,
                Detection=r.Broad_detection_method, Start=r.Sampling_start_date, Sample_name=r.Sample_name, reported_min_um=r.reported_min_size, reported_max_um=r.reported_max_size,
                reported_conc_m3=r.MP_Nconc_reported, a_cpsd=r.cumsum_slope, a_cpsd_SE=r.cumsum_slope_SD, a_psd=r.cumsum_powerlaw_alpha, log10_b=r.cumsum_log_intercept,
                lod_lo=r.lod_lo, lod_hi=r.lod_hi, MPsizeBase_1_5000_mean=r.cumsum_MP_1_5000_mean, MPsizeBase_1_5000_low=r.cumsum_MP_1_5000_low, MPsizeBase_1_5000_high=r.cumsum_MP_1_5000_high,
                PSD_MP_count=r.PSD_MP_count)
    for k, (lo, hi) in W.items():
        cover = (r.reported_min_size <= lo) and (pd.notna(r.reported_max_size) and r.reported_max_size >= hi)
        d, pf = direct(r.PSD_ID, lo, hi)
        base[f"direct_{k}"] = d if cover else np.nan
        base[f"direct_{k}_partialbin_share"] = pf if cover else np.nan
        base[f"fit_{k}"] = 10**r.cumsum_log_intercept * (lo**r.cumsum_slope - hi**r.cumsum_slope)
        base[f"fit_{k}_in_LOD"] = bool(pd.notna(r.lod_lo) and lo >= r.lod_lo and hi <= r.lod_hi)
    rows.append(base)
A = pd.DataFrame(rows)
A.to_csv(os.path.join(out, "mpsizebase_ocean_psd_windows_v1.csv"), index=False)
# sample-level totals (sum across shapes). Sample key = study+location+coords+date+depth+sample name (PSD_ID letters are site/group codes, not shapes)
key = ["Study","Location","Lat","Lon","Start","Depth_inf","Depth_sup","Sample_name","Sub_type1","Sampling","Mesh_um","Volume_m3"]
A["_k"] = A[key].map(str).agg("|".join, axis=1)  # .map(str) (not astype(str)): pandas>=3 keeps NaN as float under astype(str)
agg = {c: "sum" for c in A.columns if c.startswith(("direct_1","direct_5","direct_3","fit_")) and not c.endswith(("partialbin_share","in_LOD"))}
Sm = A.groupby("_k").agg(Study=("Study","first"), Year=("Year","first"), Location=("Location","first"), Lat=("Lat","first"), Lon=("Lon","first"), Sub_type1=("Sub_type1","first"),
                         Sub_type2=("Sub_type2","first"), Depth_inf=("Depth_inf","first"), Depth_sup=("Depth_sup","first"), Sampling=("Sampling","first"), Mesh_um=("Mesh_um","first"),
                         shapes=("MP_shape", lambda v: "+".join(sorted(set(v)))), n_psd=("PSD_ID","size"), reported_min_um=("reported_min_um","max"), reported_max_um=("reported_max_um","min"),
                         reported_conc_m3=("reported_conc_m3","sum"), **{c: (c, "sum") for c in agg}).reset_index(drop=True)
# direct sums are only valid if ALL shapes' PSDs cover the window: recompute with all-or-nothing
for k in W:
    ok = A.groupby("_k")[f"direct_{k}"].apply(lambda s: s.notna().all()).values
    Sm[f"direct_{k}"] = np.where(ok, Sm[f"direct_{k}"], np.nan)
Sm.to_csv(os.path.join(out, "mpsizebase_ocean_sample_windows_v1.csv"), index=False)
# console summary
print("ocean PSDs", len(A), "| studies", A.Study.nunique(), "| sample keys", len(Sm), "| shapes per sample:", Sm.shapes.value_counts().to_dict())
print("\nPSDs with DIRECT (observed-bin) value per window, all / pump+CTD+ISpump / tow:")
for k in W:
    v = A[f"direct_{k}"].notna()
    print(f"  {k:>9}: all {v.sum():3d} | pump-type {(v & A.Sampling.isin(['pump','CTD','IS pump'])).sum():3d} | tow {(v & (A.Sampling=='tow')).sum():3d}")
cs = A[(A.Sub_type1=="Ocean surface") & (A.Sub_type2=="coastal")]
print("\nCoastal surface PSDs:", len(cs), "| by sampling:", cs.Sampling.value_counts().to_dict(), "| with direct 50-500:", int(cs.direct_50_500.notna().sum()))
x = A[A.direct_50_500.notna() & (A.direct_50_500 > 0)].copy(); x["fit_over_direct"] = x.fit_50_500 / x.direct_50_500
print("\nfit/direct for 50-500 (PSDs where both exist):", x.fit_over_direct.describe(percentiles=[.1,.5,.9]).round(2)[["count","10%","50%","90%"]].to_dict())
print("\nDirect 50-500 um (MP/m3), coastal-surface pump PSDs:")
t = cs[cs.direct_50_500.notna() & (cs.Sampling=="pump")].groupby("Study").direct_50_500.agg(["count","min","median","max"]).round(1); print(t.to_string())
