library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(scales)

# ==============================================================================
# 1. 样式配置中心 (Style Configuration)
# ==============================================================================

# --- 颜色方案 ---
col_bg <- "#0f172a" # 背景色 (深蓝黑)
col_grid <- "#ffffff" # 网格线颜色
col_border <- "#0f172a" # 国家边框颜色 (与背景融合)
col_panel_border <- "#ffffff" # 地图区域外框颜色
col_title <- "#f5f9ff" # 主标题颜色
col_sub <- "#cbd5e1" # 副标题颜色
col_cap <- "#475569" # 说明文字颜色
col_leg_title <- "#f5f9ff" # 图例标题颜色
col_leg_text <- "#cbd5e1" # 图例文字颜色

# --- 字体设置 ---
font_main <- "Arial Black"
font_sec <- "Arial"

# --- 尺寸与间距 ---
size_title <- 45
size_sub <- 14
size_cap <- 8
size_leg_title <- 9
size_leg_text <- 8

# --- 网格线样式 ---
grid_linetype <- "solid"
grid_linewidth <- 0.4
grid_alpha <- 0.2

# --- 布局边距 (单位: pt) ---
margin_plot <- margin(20, 20, 20, 20) # 整体画布边距
margin_title <- margin(t = 40, b = 5, l = 0) # 标题左对齐，无左边距
margin_sub <- margin(b = 40, l = 0) # 副标题左对齐，无左边距
margin_cap <- margin(t = 20, l = 0, r = 0) # 【修改】说明文字：上下左右边距清零，靠 hjust 定位
margin_leg_title <- margin(b = 5)

# --- 图例位置 (相对坐标 x, y) ---
leg_pos_x <- 0.15
leg_pos_y <- 0.75

# ==============================================================================
# 2. 数据准备 (Data Preparation)
# ==============================================================================

world <- ne_countries(scale = "medium", returnclass = "sf")
europe <- world[world$continent == "Europe", ] |>
  st_transform(crs = "+proj=laea +lat_0=52 +lon_0=10 +datum=WGS84")

grat <- st_graticule(europe, ndiscr = 100)

# ==============================================================================
# 3. 绘图逻辑 (Plotting Logic)
# ==============================================================================

poster_map <- ggplot() +

  # 图层 1: 经纬度网格
  geom_sf(
    data = grat,
    color = col_grid,
    linetype = grid_linetype,
    linewidth = grid_linewidth,
    alpha = grid_alpha
  ) +

  # 图层 2: 欧洲地图主体
  geom_sf(
    data = europe,
    aes(fill = pop_est),
    color = col_border,
    size = 0.5,
    alpha = 0.95
  ) +

  # 颜色映射与图例标签优化
  scale_fill_viridis_c(
    option = "mako",
    trans = "log10",
    name = "POPULATION",
    labels = label_number(
      scale_cut = cut_short_scale(),
      accuracy = 1
    ),
    guide = guide_colorbar(
      barheight = unit(5, "cm"),
      barwidth = unit(0.5, "cm"),
      title.position = "top",
      title.hjust = 0.5,
      label.hjust = 0.5,
      frame.colour = NA,
      draw.ulim = TRUE,
      draw.llim = TRUE
    )
  ) +

  # 消除坐标轴内边距
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +

  # 文本标签
  labs(
    title = "EUROPEAN\nDEMOGRAPHICS",
    subtitle = "A spatial analysis of population density across the continent",
    caption = "Cartographer: Zayn • Source: Natural Earth Data • Projection: LAEA"
  ) +

  # 主题与样式应用
  theme_void() +
  theme(
    # 背景
    plot.background = element_rect(fill = col_bg, color = NA),

    # 地图区域边框
    panel.border = element_rect(
      color = col_panel_border,
      linewidth = 0.4,
      linetype = "solid"
    ),

    # 标题 - 严格左对齐
    plot.title = element_text(
      family = font_main,
      face = "bold",
      colour = col_title,
      size = size_title,
      hjust = 0,
      margin = margin_title
    ),

    # 副标题 - 严格左对齐
    plot.subtitle = element_text(
      family = font_sec,
      face = "plain",
      colour = col_sub,
      size = size_sub,
      hjust = 0,
      margin = margin_sub
    ),

    # 图例
    legend.position = c(leg_pos_x, leg_pos_y),
    legend.justification = "center",
    legend.title = element_text(
      family = font_sec,
      colour = col_leg_title,
      size = size_leg_title,
      face = "bold",
      hjust = 0.5,
      margin = margin_leg_title
    ),
    legend.text = element_text(
      family = font_sec,
      colour = col_leg_text,
      size = size_leg_text,
      face = "plain"
    ),

    # 【关键修改】说明文字 - 严格右对齐
    plot.caption = element_text(
      family = font_sec,
      colour = col_cap,
      size = size_cap,
      hjust = 1, # 1 代表右对齐
      margin = margin_cap # 边距已清零，靠全局 plot.margin 控制位置
    ),

    # 整体边距
    plot.margin = margin_plot
  )

print(poster_map)

# ==============================================================================
# 4. 导出图片 (Export)
# ==============================================================================
ggsave(
  # .png
  filename = "D:\\RDirectory\\viz\\laea-demographics\\europe_demographics_poster.png",
  plot = poster_map,
  width = 8,
  height = 10,
  dpi = 300
)
ggsave(
  # .svg
  filename = "D:\\RDirectory\\viz\\laea-demographics\\europe_demographics_poster.svg",
  plot = poster_map,
  width = 8,
  height = 10,
  device = "svg"
)
