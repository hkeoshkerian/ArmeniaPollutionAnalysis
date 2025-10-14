import pandas as pd
import rasterio
import numpy as np
import os

def create_dataframe_from_local_files():
    """Create DataFrame from your local TIFF files with correct Yerevan extraction - TOTALS ONLY"""
    
    # Age group descriptions for total population (both sexes only)
    age_descriptions = {
        't_00': '0 years', 't_01': '1-4 years', 't_05': '5-9 years', 't_10': '10-14 years',
        't_15': '15-19 years', 't_20': '20-24 years', 't_25': '25-29 years', 't_30': '30-34 years',
        't_35': '35-39 years', 't_40': '40-44 years', 't_45': '45-49 years', 't_50': '50-54 years',
        't_55': '55-59 years', 't_60': '60-64 years', 't_65': '65-69 years', 't_70': '70-74 years',
        't_75': '75-79 years', 't_80': '80-84 years', 't_85': '85-89 years', 't_90': '90+ years'
    }
    
    # Yerevan bounds [min_lon, min_lat, max_lon, max_lat]
    yerevan_bounds = [44.40, 40.07, 44.60, 40.24]
    
    all_data = []
    total_population_all_files = 0
    
    print("EXTRACTING YEREVAN AGE DATA (TOTALS ONLY):")
    print("=" * 50)
    
    # Process only total population files (t_*)
    for age_code, age_name in age_descriptions.items():
        filename = f"arm_{age_code}_2025_CN_100m_R2025A_v1.tif"
        file_path = os.path.join('arm_age_data', filename)
        
        if not os.path.exists(file_path):
            print(f"File not found: {filename}")
            continue
            
        try:
            with rasterio.open(file_path) as src:
                data = src.read(1)
                transform = src.transform
                
                # Extract pixels within Yerevan bounds
                yerevan_total = 0
                populated_pixels = 0
                
                rows, cols = np.where(data > 0)  # Only populated pixels
                
                for row, col in zip(rows, cols):
                    population = data[row, col]
                    lon, lat = transform * (col, row)
                    
                    # Check if within Yerevan bounds
                    if (yerevan_bounds[0] <= lon <= yerevan_bounds[2] and 
                        yerevan_bounds[1] <= lat <= yerevan_bounds[3]):
                        
                        yerevan_total += population
                        populated_pixels += 1
                        
                        all_data.append({
                            'longitude': lon,
                            'latitude': lat,
                            'population': float(population),
                            'age_cohort': age_code,
                            'age_group': age_name,
                            'sex': 'Both'  # All data is total population (both sexes)
                        })
                
                total_population_all_files += yerevan_total
                print(f"✓ {age_name:12}: {yerevan_total:6,.0f} people ({populated_pixels} pixels)")
                
        except Exception as e:
            print(f"✗ Error processing {filename}: {e}")
    
    # Create DataFrame
    if all_data:
        df = pd.DataFrame(all_data)
        df.to_csv('yerevan_age_data_totals.csv', index=False)
        
        print(f"\n DataFrame created!")
        print(f"CSV saved: yerevan_age_data_totals.csv")
        print(f"Total records: {len(df):,}")
        print(f"Total population in Yerevan: {df['population'].sum():,.0f}")
        print(f"Total across all files: {total_population_all_files:,.0f}")
        
        # Quick summary
        print(f"\nData Summary:")
        print(f"   Age groups: {df['age_group'].nunique()}")
        print(f"   Sex: {df['sex'].unique()[0]}")
        print(f"   Spatial coverage: {df['longitude'].min():.3f}° to {df['longitude'].max():.3f}°E, "
              f"{df['latitude'].min():.3f}° to {df['latitude'].max():.3f}°N")
         
        return df
    else:
        print("No data extracted for Yerevan")
        return pd.DataFrame()

# CREATE THE DATAFRAME FIRST
print("Step 1: Creating DataFrame from local files (Totals Only)...")
df = create_dataframe_from_local_files()

if len(df) > 0:
    print("\nDataFrame preview:")
    print(df.head())
else:
    print("No data available")


import plotly.express as px
import plotly.graph_objects as go
from plotly.subplots import make_subplots


def create_interactive_dashboard(df):
    """Create comprehensive interactive dashboard - TOTALS ONLY"""
    
    # Yerevan districts with their approximate boundaries [min_lon, max_lon, min_lat, max_lat]
    districts = {
    'Ajapnyak':       [44.42, 44.52, 40.16, 40.23],
    'Avan':           [44.50, 44.62, 40.20, 40.24],
    'Arabkir':        [44.45, 44.53, 40.19, 40.23],
    'Davtashen':      [44.46, 44.54, 40.22, 40.26],
    'Erebuni':        [44.52, 44.60, 40.12, 40.18],
    'Kentron':        [44.48, 44.53, 40.15, 40.20],
    'Malatia-Sebastia':[44.38, 44.48, 40.12, 40.18],
    'Nor Nork':       [44.55, 44.63, 40.16, 40.21],
    'Nork-Marash':    [44.50, 44.58, 40.16, 40.20],
    'Nubarashen':     [44.45, 44.52, 40.10, 40.15],
    'Shengavit':      [44.44, 44.53, 40.13, 40.19],
    'Kanaker-Zeytun': [44.52, 44.60, 40.18, 40.23],
    }
    
    # Calculate total population for the title
    total_yerevan_population = df['population'].sum()
    
    # Create subplot layout
    fig = make_subplots(
        rows=2, cols=2,
        specs=[[{"type": "scatter"}, {"type": "bar"}],
               [{"type": "heatmap"}, {"type": "pie"}]],
        subplot_titles=('Population Distribution by Age', 'Top Age Groups by Population', 
                       'Population Density Heatmap', 'Age Structure Distribution'),
        vertical_spacing=0.15,  # Increased spacing
        horizontal_spacing=0.15  # Increased spacing
    )
    
    # 1. Scatter plot - Show different age groups with colors
    if len(df) > 0:
        # Sample data for better performance
        sample_df = df.sample(n=min(5000, len(df)), random_state=42)
        
        # Create color mapping for major age groups
        def get_age_category(age_group):
            if any(x in age_group for x in ['0 years', '1-4 years', '5-9 years', '10-14 years']):
                return 'Children (0-14)'
            elif any(x in age_group for x in ['15-19 years', '20-24 years']):
                return 'Youth (15-24)'
            elif any(x in age_group for x in ['25-29 years', '30-34 years', '35-39 years', '40-44 years']):
                return 'Young Adults (25-44)'
            elif any(x in age_group for x in ['45-49 years', '50-54 years', '55-59 years', '60-64 years']):
                return 'Middle Age (45-64)'
            else:
                return 'Elderly (65+)'
        
        sample_df['age_category'] = sample_df['age_group'].apply(get_age_category)
        color_map = {
            'Children (0-14)': '#1f77b4', 
            'Youth (15-24)': '#2ca02c', 
            'Young Adults (25-44)': '#ff7f0e', 
            'Middle Age (45-64)': '#d62728', 
            'Elderly (65+)': '#9467bd'
        }
        
        for category, color in color_map.items():
            category_data = sample_df[sample_df['age_category'] == category]
            if len(category_data) > 0:
                fig.add_trace(
                    go.Scatter(
                        x=category_data['longitude'],
                        y=category_data['latitude'],
                        mode='markers',
                        marker=dict(
                            size=3,
                            color=color,
                            opacity=0.6
                        ),
                        name=category,
                        text=category_data.apply(lambda row: f"{row['age_group']}<br>Pop: {row['population']:.1f}", axis=1),
                        hovertemplate='<b>%{text}</b><extra></extra>',
                        showlegend=True
                    ),
                    row=1, col=1
                )
        
        # Add district boundaries as rectangles
        district_colors = px.colors.qualitative.Set3
        for i, (district, bounds) in enumerate(districts.items()):
            fig.add_trace(
                go.Scatter(
                    x=[bounds[0], bounds[1], bounds[1], bounds[0], bounds[0]],
                    y=[bounds[2], bounds[2], bounds[3], bounds[3], bounds[2]],
                    mode='lines',
                    line=dict(color=district_colors[i % len(district_colors)], width=2),
                    fill='toself',
                    fillcolor=district_colors[i % len(district_colors)].replace('rgb', 'rgba').replace(')', ', 0.2)'),
                    text=district,
                    hovertemplate=f'<b>{district}</b><extra></extra>',
                    showlegend=False  # Don't show in legend to avoid clutter
                ),
                row=1, col=1
            )
            
            # Add district labels
            center_lon = (bounds[0] + bounds[1]) / 2
            center_lat = (bounds[2] + bounds[3]) / 2
            
            fig.add_annotation(
                x=center_lon,
                y=center_lat,
                text=district,
                showarrow=False,
                font=dict(size=9, color="black", family="Arial"),
                bgcolor="white",
                bordercolor="black",
                borderwidth=1,
                borderpad=2,
                opacity=0.8,
                row=1, col=1
            )
    
    # 2. Bar chart - Top age groups by population
    age_totals = df.groupby('age_group')['population'].sum().sort_values(ascending=True).tail(15)
    fig.add_trace(
        go.Bar(
            x=age_totals.values,
            y=age_totals.index,
            orientation='h',
            marker_color='lightblue',
            name='Population by Age',
            hovertemplate='<b>%{y}</b><br>Population: %{x:,}<extra></extra>',
            showlegend=False  # Don't show in main legend
        ),
        row=1, col=2
    )
    
    # 3. Heatmap - Population density
    if len(df) > 0:
        lat_bins = pd.cut(df['latitude'], bins=15)
        lon_bins = pd.cut(df['longitude'], bins=15)
        
        pivot_data = df.groupby([lat_bins, lon_bins])['population'].sum().unstack(fill_value=0)
        
        fig.add_trace(
            go.Heatmap(
                z=pivot_data.values,
                x=[f"{interval.mid:.3f}°" for interval in pivot_data.columns],
                y=[f"{interval.mid:.3f}°" for interval in pivot_data.index],
                colorscale='YlOrRd',
                name='Population Density',
                hovertemplate='Lat: %{y}<br>Lon: %{x}<br>Population: %{z:,}<extra></extra>',
                showlegend=False  # Don't show in main legend
            ),
            row=2, col=1
        )
    
    # 4. Pie chart - Age structure distribution
    if len(df) > 0:
        segments = {
            'Children (0-14)': df[df['age_group'].isin(['0 years', '1-4 years', '5-9 years', '10-14 years'])]['population'].sum(),
            'Youth (15-24)': df[df['age_group'].isin(['15-19 years', '20-24 years'])]['population'].sum(),
            'Young Adults (25-44)': df[df['age_group'].isin(['25-29 years', '30-34 years', '35-39 years', '40-44 years'])]['population'].sum(),
            'Middle Age (45-64)': df[df['age_group'].isin(['45-49 years', '50-54 years', '55-59 years', '60-64 years'])]['population'].sum(),
            'Elderly (65+)': df[df['age_group'].isin(['65-69 years', '70-74 years', '75-79 years', '80-84 years', '85-89 years', '90+ years'])]['population'].sum()
        }
        
        # Only include segments with data
        segments = {k: v for k, v in segments.items() if v > 0}
        
        if segments:
            fig.add_trace(
                go.Pie(
                    labels=list(segments.keys()),
                    values=list(segments.values()),
                    name='Age Structure',
                    hovertemplate='<b>%{label}</b><br>Population: %{value:,}<br>Percentage: %{percent}<extra></extra>',
                    showlegend=False  # Pie chart shows its own legend
                ),
                row=2, col=2
            )
    
    # Update layout with better legend positioning
    fig.update_layout(
        title_text=f"Yerevan Population Age Structure - 2025 Projection<br><sub>Total Population: {total_yerevan_population:,.0f} people - 100m Resolution</sub>",
        height=1000,  # Increased height for better spacing
        showlegend=True,
        legend=dict(
            orientation="v",
            yanchor="top",
            y=0.98,
            xanchor="left",
            x=1.02,  # Position legend to the right of the plots
            bgcolor='rgba(255,255,255,0.8)',
            bordercolor='black',
            borderwidth=1
        )
    )
    
    # Update axes labels
    fig.update_xaxes(title_text="Longitude", row=1, col=1)
    fig.update_yaxes(title_text="Latitude", row=1, col=1)
    fig.update_xaxes(title_text="Population", row=1, col=2)
    fig.update_xaxes(title_text="Longitude", row=2, col=1)
    fig.update_yaxes(title_text="Latitude", row=2, col=1)
    
    # Add district information as a separate annotation
    fig.add_annotation(
        x=0.5,
        y=-0.1,
        xref="paper",
        yref="paper",
        text="Yerevan Districts: " + ", ".join(districts.keys()),
        showarrow=False,
        font=dict(size=10),
        align="center"
    )
    
    fig.write_html("yerevan_population_dashboard_totals.html")
    print("Dashboard saved as: yerevan_population_dashboard_totals.html")
    return fig

# CREATE THE DASHBOARD
if len(df) > 0:
    print("\nStep 2: Creating interactive dashboard...")
    dashboard = create_interactive_dashboard(df)
    print("Dashboard created successfully! Open 'yerevan_population_dashboard_totals.html' in your browser.")
else:
    print("No data available to create dashboard")
