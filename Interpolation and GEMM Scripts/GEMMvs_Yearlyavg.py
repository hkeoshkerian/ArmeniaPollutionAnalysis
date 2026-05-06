# ====================================================================
# GEMM Implementation - Annual Average PM2.5 with Age-Cohort Outputs
# Python translation of R script
# Libraries: rioxarray, rasterio, geopandas, pandas, numpy,
#            matplotlib, contextily, folium, openpyxl
# ====================================================================

import os
import time
import warnings
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.colors as mcolors
import matplotlib.cm as cm
import contextily as ctx
import folium
import rasterio
import rioxarray as rxr
import xarray as xr
from rasterio.enums import Resampling
from rasterio.warp import reproject, Resampling as WarpResampling, calculate_default_transform
from scipy.ndimage import zoom as ndimage_zoom
from scipy.stats import loess   # use statsmodels instead — see below
import statsmodels.api as sm
import geopandas as gpd
from shapely.geometry import box

warnings.filterwarnings("ignore")

# ====================================================================
# File paths
# ====================================================================
pop_path  = r"C:/Users/mrealehatem/Documents/GitHub/ArmeniaPollutionAnalysis/WorldPopData/arm_age_data/pop.tif"
pm25_path = r"C:/Users/mrealehatem/OneDrive/AUA/Air pollution/pm25_yerevan_stack.tif"
var_path  = r"C:/Users/mrealehatem/OneDrive/AUA/Air pollution/pm25_variance_stack.tif"
coef_path = r"C:/Users/mrealehatem/Documents/GitHub/ArmeniaPollutionAnalysis/GEMMcoefficients.xlsx"
out_dir   = r"C:/Users/mrealehatem/Documents/GitHub/ArmeniaPollutionAnalysis"

os.makedirs(out_dir, exist_ok=True)

# ====================================================================
# Helper: reproject a rioxarray DataArray to match a target CRS/grid
# ====================================================================
def reproject_match(src_da, target_da, resampling=Resampling.bilinear):
    """Reproject src_da to match target_da's CRS, transform, and shape."""
    return src_da.rio.reproject_match(target_da, resampling=resampling)


def raster_to_wgs84(da):
    """Reproject a rioxarray DataArray to EPSG:4326."""
    return da.rio.reproject("EPSG:4326", resampling=Resampling.bilinear)


# ====================================================================
# Load data
# ====================================================================
print("Loading data...")

# Population raster — multi-band (one band per age group)
pop_ds  = rxr.open_rasterio(pop_path, masked=True)   # shape: (bands, y, x)
pm25_ds = rxr.open_rasterio(pm25_path, masked=True)  # shape: (bands, y, x)

# Harmonise CRS
if pop_ds.rio.crs != pm25_ds.rio.crs:
    print("Reprojecting population raster to match PM2.5 CRS...")
    pop_ds = pop_ds.rio.reproject(pm25_ds.rio.crs)

# Clip population to PM2.5 extent
pop_ds = pop_ds.rio.clip_box(*pm25_ds.rio.bounds())

# ── Band-name mapping ────────────────────────────────────────────────
# rioxarray uses integer band indices by default; recover names from
# the GeoTIFF descriptions if available, otherwise fall back to the
# long_name attribute or positional labels.
with rasterio.open(pop_path) as src:
    band_descriptions = list(src.descriptions)   # may be None per band

# Build a dict  {band_name: band_index_0based}
if all(d is not None and d != "" for d in band_descriptions):
    pop_band_names = {desc: i for i, desc in enumerate(band_descriptions)}
else:
    # Fallback: assume bands are labelled "0-9", "1-10", … in order
    pop_band_names = {str(i): i for i in range(pop_ds.shape[0])}

print(f"Population bands found: {list(pop_band_names.keys())}")

# ====================================================================
# Load coefficients
# ====================================================================
print("Loading coefficients...")
coeff_all = pd.read_excel(coef_path, header=0)
coeff_all.columns = ["Cause","AgeGroup","Theta","Theta_SD","Alpha","Mu","Nu",
                      "Deaths23","Pop23","DeathRate"]
coeff_all = coeff_all.dropna()
coeff_df  = coeff_all[coeff_all["Cause"] == "NCD+LRI"].reset_index(drop=True)

if coeff_df.empty:
    raise ValueError("No rows found for Cause == 'NCD+LRI'.")

# ====================================================================
# Age mapping
# ====================================================================
age_map = {
    "0-9":"0-25","1-10":"0-25","5-14":"0-25","10-19":"0-25","15-24":"0-25",
    "20-29":"25-30","25-34":"30-35","30-39":"35-40","35-44":"40-45","40-49":"45-50",
    "45-54":"50-55","50-59":"55-60","55-64":"60-65","60-69":"65-70","65-74":"70-75",
    "70-79":"75-80","75-84":"80+","80-89":"80+","85-94":"80+","90-99":"80+"
}

age_order = ["0-25","25-30","30-35","35-40","40-45","45-50","50-55","55-60",
             "60-65","65-70","70-75","75-80","80+"]

# ====================================================================
# Coefficient matrix  →  dict[cause][age] = {theta, theta_sd, alpha, mu, nu}
# ====================================================================
causes_list = coeff_df["Cause"].unique().tolist()
coef_ages   = coeff_df["AgeGroup"].unique().tolist()

coef_matrix = {}
for cause in causes_list:
    coef_matrix[cause] = {}
    for age in coef_ages:
        row = coeff_df[(coeff_df["Cause"] == cause) & (coeff_df["AgeGroup"] == age)]
        if not row.empty:
            r = row.iloc[0]
            coef_matrix[cause][age] = dict(
                theta=r["Theta"], theta_sd=r["Theta_SD"],
                alpha=r["Alpha"], mu=r["Mu"], nu=r["Nu"]
            )

# ====================================================================
# GEMM function
# ====================================================================
def calculate_gemm_rr(pm25, theta, alpha, mu, nu, cf=2.4):
    """Vectorised GEMM relative-risk calculation."""
    z     = np.where(pm25 - cf < 0, 0.0, pm25 - cf)
    omega = 1.0 / (1.0 + np.exp(-(z - mu) / nu))
    return np.exp(theta * np.log(z / alpha + 1) * omega)


baseline_rate = 744.03 / 100_000

# ====================================================================
# Step 1: Annual average PM2.5 resampled to population grid resolution
# ====================================================================
print("Calculating annual average PM2.5...")
t0 = time.time()

# Use the first population band as the reference grid
pop_ref = pop_ds.isel(band=0)

# Resample every PM2.5 band to pop grid, then average across time bands
pm25_resampled  = pm25_ds.rio.reproject_match(pop_ref, resampling=Resampling.bilinear)
pm25_annual_avg = pm25_resampled.mean(dim="band")   # shape: (y, x)

# ====================================================================
# Step 2: Mortality loop
# ====================================================================
print("Calculating excess mortality...")

death_rates = coeff_df.set_index("AgeGroup")["DeathRate"].to_dict()

n_cells = pm25_annual_avg.values.size
deaths_by_cell = np.zeros(n_cells)
var_by_cell    = np.zeros(n_cells)
pm25_vals      = pm25_annual_avg.values.ravel()

age_deaths_rows = []

for ag, band_idx in pop_band_names.items():
    if ag not in age_map:
        print(f"  ⚠ No mapping: {ag}")
        continue

    pop_vals  = pop_ds.isel(band=band_idx).values.ravel().astype(float)
    coef_age  = age_map[ag]

    if coef_age not in death_rates or np.isnan(death_rates[coef_age]):
        print(f"  ⚠ No death rate: {coef_age}")
        continue

    age_dr = death_rates[coef_age]

    if coef_age not in coef_matrix.get("NCD+LRI", {}):
        print(f"  ⚠ No coefficients: {coef_age}")
        continue

    c = coef_matrix["NCD+LRI"][coef_age]

    rr_vals  = calculate_gemm_rr(pm25_vals, c["theta"], c["alpha"], c["mu"], c["nu"])
    paf_vals = np.where(np.isnan(rr_vals), 0.0, (rr_vals - 1) / rr_vals)

    baseline_death_vals = np.nan_to_num(pop_vals * age_dr)
    death_vals          = np.nan_to_num(paf_vals * baseline_death_vals)
    var_vals            = np.nan_to_num(baseline_death_vals**2 * c["theta_sd"]**2)

    deaths_by_cell += death_vals
    var_by_cell    += var_vals

    age_deaths_rows.append(dict(
        RasterAgeGroup   = ag,
        CoefAge          = coef_age,
        Deaths           = np.nansum(death_vals),
        BaselineDeaths   = np.nansum(baseline_death_vals),
        MeanRR           = np.nanmean(rr_vals),
        MeanPAF          = np.nanmean(paf_vals),
        MeanAbsoluteRisk = np.nanmean(paf_vals * age_dr),
    ))
    print(f"  ✓ {ag} -> {coef_age} | death rate = {age_dr:.6f}")

# ── Aggregate by CoefAge ─────────────────────────────────────────────
age_deaths_summary = pd.DataFrame(age_deaths_rows)

age_deaths_agg = (
    age_deaths_summary
    .groupby("CoefAge", as_index=False)
    .agg(Deaths=("Deaths","sum"), BaselineDeaths=("BaselineDeaths","sum"),
         MeanRR=("MeanRR","mean"), MeanPAF=("MeanPAF","mean"),
         MeanAbsoluteRisk=("MeanAbsoluteRisk","mean"))
)
age_deaths_agg["CoefAge"] = pd.Categorical(
    age_deaths_agg["CoefAge"], categories=age_order, ordered=True
)
age_deaths_agg = age_deaths_agg.sort_values("CoefAge")

cause_deaths_summary = pd.DataFrame([{
    "Cause": "NCD+LRI",
    "Deaths": age_deaths_summary["Deaths"].sum(),
    "BaselineDeaths": age_deaths_summary["BaselineDeaths"].sum(),
}])

# ── Rebuild spatial rasters from flat arrays ─────────────────────────
shape_2d = pm25_annual_avg.values.shape  # (y, x)

def flat_to_raster(arr, template_da):
    """Reshape a flat numpy array back into a rioxarray DataArray."""
    da = template_da.copy(data=arr.reshape(shape_2d))
    return da

deaths_raster = flat_to_raster(deaths_by_cell, pm25_annual_avg)
var_raster    = flat_to_raster(var_by_cell,    pm25_annual_avg)
se_raster     = flat_to_raster(np.sqrt(var_by_cell), pm25_annual_avg)

elapsed = time.time() - t0
print(f"Done in {elapsed/60:.1f} minutes")

# ====================================================================
# Step 3: Variance stack
# ====================================================================
var_stack_ds = rxr.open_rasterio(var_path, masked=True)
if var_stack_ds.rio.crs != pm25_ds.rio.crs:
    var_stack_ds = var_stack_ds.rio.reproject(pm25_ds.rio.crs)
var_pop_grid = var_stack_ds.rio.reproject_match(
    pop_ref, resampling=Resampling.bilinear
).mean(dim="band")

# ====================================================================
# Step 4: Summary statistics
# ====================================================================
total_deaths = np.nansum(deaths_by_cell)
total_se     = np.sqrt(np.nansum(var_by_cell))
ci_lower     = total_deaths - 1.96 * total_se
ci_upper     = total_deaths + 1.96 * total_se

print("\n=== Annual Excess Mortality Results ===")
print(f"Total Deaths : {total_deaths:.2f}")
print(f"SE           : {total_se:.2f}")
print(f"95% CI       : [{ci_lower:.2f}, {ci_upper:.2f}]")

results_df = pd.DataFrame({
    "Metric": ["Total Deaths","SE","CI Lower","CI Upper"],
    "Value":  [round(v, 2) for v in [total_deaths, total_se, ci_lower, ci_upper]]
})
print(results_df)

results_df.to_csv(os.path.join(out_dir, "yerevan_annual_mortality.csv"),       index=False)
age_deaths_summary.to_csv(os.path.join(out_dir, "age_deaths_summary_raw.csv"), index=False)
age_deaths_agg.to_csv(os.path.join(out_dir, "age_deaths_summary_aggregated.csv"), index=False)
cause_deaths_summary.to_csv(os.path.join(out_dir, "cause_deaths_summary.csv"), index=False)

# ====================================================================
# Step 5: Interactive Folium map  (replaces leaflet)
# ====================================================================
pop_total_vals = np.nansum(
    np.stack([pop_ds.isel(band=i).values for i in range(pop_ds.shape[0])], axis=0),
    axis=0
).ravel()

deaths_per_cap_vals = np.where(
    (pop_total_vals > 0) & (deaths_by_cell > 0),
    (deaths_by_cell / pop_total_vals) * 1000,
    np.nan
)
# Mask cells with no PM2.5
deaths_per_cap_vals = np.where(np.isnan(pm25_vals), np.nan, deaths_per_cap_vals)

deaths_per_capita = flat_to_raster(deaths_per_cap_vals, pm25_annual_avg)
deaths_per_capita_wgs84 = raster_to_wgs84(deaths_per_capita)

# ── Save as GeoTIFF for Folium raster overlay ────────────────────────
tmp_tif = os.path.join(out_dir, "_deaths_per_capita_wgs84.tif")
deaths_per_capita_wgs84.rio.to_raster(tmp_tif)

# Build Folium map with an image overlay
dpc_vals = deaths_per_capita_wgs84.values
vmin     = np.nanmin(dpc_vals)
vmax     = np.nanmax(dpc_vals)

norm  = mcolors.Normalize(vmin=vmin, vmax=vmax)
cmap  = cm.get_cmap("viridis")
rgba  = cmap(norm(np.where(np.isnan(dpc_vals), 0, dpc_vals)))
rgba[..., 3] = np.where(np.isnan(dpc_vals), 0, 0.7)   # alpha mask

import matplotlib
png_tmp = os.path.join(out_dir, "_overlay_tmp.png")
matplotlib.image.imsave(png_tmp, rgba)

bounds_wgs84 = deaths_per_capita_wgs84.rio.bounds()  # (left, bottom, right, top)
image_bounds  = [[bounds_wgs84[1], bounds_wgs84[0]],  # SW
                 [bounds_wgs84[3], bounds_wgs84[2]]]  # NE

m = folium.Map(location=[40.1792, 44.5086], zoom_start=12,
               tiles="OpenStreetMap")
folium.raster_layers.ImageOverlay(
    image=png_tmp,
    bounds=image_bounds,
    opacity=0.7,
    name="Excess Deaths per 1,000"
).add_to(m)
folium.LayerControl().add_to(m)
m.save(os.path.join(out_dir, "mortality_heatmap_interactive.html"))
print("Interactive heatmap saved")

# ====================================================================
# Helper: convert rioxarray DataArray → (x, y, value) DataFrame
#         reprojected to WGS84, ready for matplotlib/contextily
# ====================================================================
def raster_to_df(da, value_name="value"):
    """Return a tidy DataFrame with columns x, y, <value_name> in WGS84."""
    da_wgs84 = raster_to_wgs84(da)
    y_coords = da_wgs84.y.values
    x_coords = da_wgs84.x.values
    xx, yy   = np.meshgrid(x_coords, y_coords)
    df = pd.DataFrame({
        "x": xx.ravel(),
        "y": yy.ravel(),
        value_name: da_wgs84.values.ravel()
    })
    return df


# ====================================================================
# Shared map-plot function using contextily for OSM basemap
# ====================================================================
def make_map_plot(df, value_col, title, cmap_name, label,
                  subtitle=None, caption=None, clip_q=None,
                  figsize=(9, 7)):
    """
    Plot a raster overlay on an OSM basemap using contextily.
    df        : DataFrame with columns x, y, value_col  (WGS84 degrees)
    clip_q    : if float, clip values at that quantile (e.g. 0.99)
    """
    vals = df[value_col].copy()
    if clip_q is not None:
        q = np.nanquantile(vals, clip_q)
        vals = vals.clip(upper=q)

    # Build a GeoDataFrame and convert to Web Mercator for contextily
    gdf = gpd.GeoDataFrame(
        {value_col: vals},
        geometry=gpd.points_from_xy(df["x"], df["y"]),
        crs="EPSG:4326"
    ).to_crs("EPSG:3857")

    # Determine pixel grid extent
    x_min, y_min, x_max, y_max = gdf.total_bounds
    resolution = 100  # metres per pixel (adjust as needed)
    nx = max(int((x_max - x_min) / resolution), 1)
    ny = max(int((y_max - y_min) / resolution), 1)

    # Build a regular grid for imshow
    xi = np.linspace(x_min, x_max, nx)
    yi = np.linspace(y_min, y_max, ny)
    from scipy.interpolate import griddata
    grid_vals = griddata(
        np.column_stack([gdf.geometry.x, gdf.geometry.y]),
        gdf[value_col].values,
        np.column_stack([v.ravel() for v in np.meshgrid(xi, yi[::-1])]),
        method="nearest"
    ).reshape(ny, nx)

    fig, ax = plt.subplots(figsize=figsize)
    norm   = mcolors.Normalize(vmin=np.nanmin(grid_vals), vmax=np.nanmax(grid_vals))
    cmap_  = cm.get_cmap(cmap_name)
    im     = ax.imshow(
        grid_vals, extent=[x_min, x_max, y_min, y_max],
        cmap=cmap_, norm=norm, alpha=0.75, origin="upper", aspect="auto"
    )
    ctx.add_basemap(ax, source=ctx.providers.OpenStreetMap.Mapnik, zoom=12)
    cbar = fig.colorbar(im, ax=ax, shrink=0.7, pad=0.02)
    cbar.set_label(label, fontsize=9)
    ax.set_title(title, fontsize=12, fontweight="bold")
    if subtitle:
        ax.set_xlabel(subtitle, fontsize=8, color="gray")
    if caption:
        fig.text(0.5, 0.01, caption, ha="center", fontsize=7, color="gray",
                 wrap=True)
    ax.tick_params(labelsize=8)
    ax.grid(False)
    plt.tight_layout()
    return fig


# ====================================================================
# PLOT 1 – Annual Average PM2.5
# ====================================================================
df1 = raster_to_df(pm25_annual_avg, "PM25")
p1  = make_map_plot(df1, "PM25",
                    title="Annual Average PM2.5 – Yerevan",
                    subtitle="OSM basemap | semi-transparent raster overlay",
                    cmap_name="YlOrRd", label="PM2.5 (µg/m³)")

# ====================================================================
# PLOT 2 – Deaths per 1,000 people
# ====================================================================
df2 = raster_to_df(deaths_per_capita, "DeathsPer1k")
p2  = make_map_plot(df2, "DeathsPer1k",
                    title="PM2.5 Excess Deaths per 1,000 – Yerevan",
                    cmap_name="viridis", label="Deaths per 1k")

# ====================================================================
# PLOT 3 – Total excess deaths per grid cell
# ====================================================================
df3 = raster_to_df(deaths_raster, "Deaths")
p3  = make_map_plot(df3, "Deaths",
                    title="Total Excess Deaths per Grid Cell – Yerevan",
                    cmap_name="Reds", label="Deaths")

# ====================================================================
# PLOT 4 – Uncertainty (SE)
# ====================================================================
df4 = raster_to_df(se_raster, "SE")
p4  = make_map_plot(df4, "SE",
                    title="Mortality Estimate Uncertainty (SE) – Yerevan",
                    cmap_name="Blues", label="SE (deaths)")

# ====================================================================
# Non-spatial plots
# ====================================================================

# p10 – PM2.5 deaths by cause
fig10, ax10 = plt.subplots(figsize=(8, 5))
cds = cause_deaths_summary.sort_values("Deaths")
ax10.barh(cds["Cause"], cds["Deaths"], color="steelblue")
ax10.set_title("PM2.5 Attributable Deaths by Cause")
ax10.set_xlabel("Attributable deaths")
plt.tight_layout()

# p11 – Baseline mortality by cause
fig11, ax11 = plt.subplots(figsize=(8, 5))
cds2 = cause_deaths_summary.sort_values("BaselineDeaths")
ax11.barh(cds2["Cause"], cds2["BaselineDeaths"], color="coral")
ax11.set_title("Baseline Mortality by Cause")
ax11.set_xlabel("Baseline deaths")
plt.tight_layout()

# p12 – PM2.5 deaths by age cohort
fig12, ax12 = plt.subplots(figsize=(9, 5.5))
ax12.bar(age_deaths_agg["CoefAge"].astype(str),
         age_deaths_agg["Deaths"], color="steelblue")
ax12.set_title("PM2.5 Attributable Deaths by Age Cohort")
ax12.set_xlabel("Age cohort"); ax12.set_ylabel("Attributable deaths")
plt.xticks(rotation=45, ha="right"); plt.tight_layout()

# p13 – GEMM absolute risk by age cohort
fig13, ax13 = plt.subplots(figsize=(9, 5.5))
ax13.plot(age_deaths_agg["CoefAge"].astype(str),
          age_deaths_agg["MeanAbsoluteRisk"], marker="o",
          linewidth=1.5, color="#2C7FB8")
ax13.set_title("GEMM Absolute Risk by Age Cohort")
ax13.set_xlabel("Age cohort"); ax13.set_ylabel("Mean absolute risk")
plt.xticks(rotation=45, ha="right"); plt.tight_layout()

# p14 – Baseline mortality by age cohort
fig14, ax14 = plt.subplots(figsize=(9, 5.5))
ax14.bar(age_deaths_agg["CoefAge"].astype(str),
         age_deaths_agg["BaselineDeaths"], color="coral")
ax14.set_title("Baseline Mortality by Age Cohort")
ax14.set_xlabel("Age cohort"); ax14.set_ylabel("Baseline deaths")
plt.xticks(rotation=45, ha="right"); plt.tight_layout()

# ---- GEMM curves ----
pm25_seq     = np.linspace(0, 100, 500)
cf           = 2.4
selected_ages = ["0-25","45-50","65-70","80+"]

curve_rows = []
for ag in selected_ages:
    if ag not in coef_matrix["NCD+LRI"]:
        continue
    c   = coef_matrix["NCD+LRI"][ag]
    rr  = calculate_gemm_rr(pm25_seq, c["theta"], c["alpha"], c["mu"], c["nu"], cf)
    paf = (rr - 1) / rr
    for i in range(len(pm25_seq)):
        curve_rows.append(dict(PM25=pm25_seq[i], RR=rr[i], PAF=paf[i], AgeGroup=ag))

curve_df = pd.DataFrame(curve_rows)

# p6a – RR vs PM2.5
fig6a, ax6a = plt.subplots(figsize=(8, 5))
for ag in selected_ages:
    sub = curve_df[curve_df["AgeGroup"] == ag]
    ax6a.plot(sub["PM25"], sub["RR"], label=ag, linewidth=1.5)
ax6a.axvline(cf, linestyle="--", color="gray")
ax6a.set_title("GEMM Relative Risk vs. PM2.5")
ax6a.set_xlabel("PM2.5 (µg/m³)"); ax6a.set_ylabel("RR")
ax6a.legend(title="Age Group"); plt.tight_layout()

# p6b – PAF vs PM2.5
fig6b, ax6b = plt.subplots(figsize=(8, 5))
for ag in selected_ages:
    sub = curve_df[curve_df["AgeGroup"] == ag]
    ax6b.plot(sub["PM25"], sub["PAF"], label=ag, linewidth=1.5)
ax6b.axvline(cf, linestyle="--", color="gray")
ax6b.set_title("PAF vs. PM2.5")
ax6b.set_xlabel("PM2.5 (µg/m³)"); ax6b.set_ylabel("PAF")
ax6b.legend(title="Age Group"); plt.tight_layout()

# p7 – Scatter PM2.5 vs excess deaths (with LOESS)
scatter_mask = (~np.isnan(pm25_vals)) & (deaths_by_cell > 0)
pm25_sc   = pm25_vals[scatter_mask]
deaths_sc = deaths_by_cell[scatter_mask]

# LOESS via statsmodels
lowess = sm.nonparametric.lowess(deaths_sc, pm25_sc, frac=0.3)

fig7, ax7 = plt.subplots(figsize=(7, 5))
ax7.scatter(pm25_sc, deaths_sc, alpha=0.4, s=6, color="tomato")
ax7.plot(lowess[:, 0], lowess[:, 1], color="black", linewidth=1.5)
ax7.set_title("Grid-Cell PM2.5 vs. Excess Deaths")
ax7.set_xlabel("PM2.5 (µg/m³)"); ax7.set_ylabel("Excess Deaths")
plt.tight_layout()

# p8 – Pie chart
pop_total_sum  = np.nansum(pop_total_vals)
baseline_total = max(pop_total_sum * baseline_rate - total_deaths, 0)
fig8, ax8 = plt.subplots(figsize=(6, 6))
ax8.pie([baseline_total, total_deaths],
        labels=["Baseline (non-PM2.5)","PM2.5 Attributable"],
        colors=["steelblue","tomato"], autopct="%1.1f%%", startangle=90)
ax8.set_title("Share of Deaths Attributable to PM2.5")
plt.tight_layout()

# p9 – CI bar chart
fig9, ax9 = plt.subplots(figsize=(5, 5))
ax9.bar(["NCD+LRI"], [total_deaths], color="steelblue", width=0.4)
ax9.errorbar(["NCD+LRI"], [total_deaths],
             yerr=[[total_deaths - ci_lower], [ci_upper - total_deaths]],
             fmt="none", color="black", linewidth=1.5, capsize=6)
ax9.set_title("PM2.5 Excess Deaths – Yerevan (95% CI)")
ax9.set_ylabel("Excess Deaths")
plt.tight_layout()

# ====================================================================
# EXTRA PLOT – Deaths per capita per µg/m³ PM2.5
# ====================================================================
pm25_grid_vals = pm25_vals.copy()
dpp_vals = np.where(
    (~np.isnan(pm25_grid_vals)) & (pm25_grid_vals > 2.4) &
    (~np.isnan(pop_total_vals)) & (pop_total_vals > 0) &
    (deaths_by_cell > 0),
    (deaths_by_cell / pop_total_vals) / pm25_grid_vals * 100_000,
    np.nan
)

deaths_per_pm25_raster = flat_to_raster(dpp_vals, pm25_annual_avg)
df_dpp = raster_to_df(deaths_per_pm25_raster, "DeathsPerCapPM25")
q99    = np.nanquantile(df_dpp["DeathsPerCapPM25"], 0.99)
df_dpp["DeathsPerCapPM25_clipped"] = df_dpp["DeathsPerCapPM25"].clip(upper=q99)

p_dpp = make_map_plot(
    df_dpp.rename(columns={"DeathsPerCapPM25_clipped": "dpp"}),
    "dpp",
    title="PM2.5 Per-Capita Mortality Efficiency – Yerevan",
    subtitle="Excess deaths per 100,000 people per µg/m³ of annual average PM2.5",
    caption=("Values above 99th percentile clipped. "
             "Cells below GEMM counterfactual (2.4 µg/m³) or with zero population excluded."),
    cmap_name="RdYlGn_r",
    label="Deaths per\n100k pop\nper µg/m³",
    clip_q=None   # already clipped above
)

# ====================================================================
# Save all figures
# ====================================================================
save_list = [
    (p1,   "plot1_pm25_map.png"),
    (p2,   "plot2_deaths_per_capita.png"),
    (p3,   "plot3_deaths_total.png"),
    (p4,   "plot4_uncertainty.png"),
    (fig10,"plot10_pm25_deaths_by_cause.png"),
    (fig11,"plot11_baseline_by_cause.png"),
    (fig12,"plot12_pm25_deaths_by_age.png"),
    (fig13,"plot13_gemm_risk_by_age.png"),
    (fig14,"plot14_baseline_by_age.png"),
    (fig6a,"plot6a_rr_curve.png"),
    (fig6b,"plot6b_paf_curve.png"),
    (fig7, "plot7_scatter.png"),
    (fig8, "plot8_pie.png"),
    (fig9, "plot9_ci_bar.png"),
    (p_dpp,"plot_deaths_per_capita_per_pm25_map.png"),
]

for fig, fname in save_list:
    fig.savefig(os.path.join(out_dir, fname), dpi=300, bbox_inches="tight")
    plt.close(fig)
    print(f"Saved: {fname}")

print("\nAll outputs saved successfully.")
