# DATA TABLE IS READY BIPLOT ANALYSIS BELOW 
# Data table was created using "makeGGEmat.R"

# for svp 2 is column preserving - environment eigenvectors (for evaluating environments)
# for svp 1 is row preserving - genotype eigenvectors (for evaluating GXE)
dataUpdated = read_xlsx("BLUPsTwoStep.xlsx")

##BASICBIPLOT
gge_model = gge(dataUpdated,env,geno,yield, centering = "2", scaling ="sd", svp="2") 
b <- plot(gge_model,
          col.gen = "orange",
          size.text.env = 2,
          axis_expand = 1,
          plot_theme = theme_metan())
arrange_ggplot(b)


#MEAN PERFORMANCE VS STABILITY
gge_model = gge(dataUpdated,env,geno,yield, centering = "2", scaling ="sd", svp="1") 

d <- plot(gge_model,
          type = 2,
          col.gen = "black",
          col.env = "red",
          axis_expand = 1.5,
          plot_theme = theme_metan_minimal())
arrange_ggplot(d)

#WHICH WON WHERE
gge_model <- gge(dataUpdated, env, geno, yield, svp = "symmetrical")
f <- plot(gge_model,
          type = 3,
          size.shape.win = 5,
          large_label = 6,
          col.gen = "orange",
          col.env = "blue")
arrange_ggplot(f)

#DISCRIMINITAVENESS VS REPRESENTATIVENESS
gge_model <- gge(dataUpdated, env, geno, yield,centering = "2", scaling="1", svp = "2")
h <- plot(gge_model,
          type = 4,
          size.text.gen = 2,
          size.text.env = 2,
          axis_expand = 1,
          plot_theme = theme_metan_minimal())
arrange_ggplot(h)

#EXAMINE AN ENVIRONMENT
gge_model <- gge(dataUpdated, env, geno, yield, svp = "symmetrical")
j <- plot(gge_model,
          type = 5,
          sel_env = "UNZA2019",
          col.gen = "black",
          col.env = "black",
          size.text.env = 5,
          axis_expand = 0.5)
arrange_ggplot(j)


#RANK ENVIRONMENTS
gge_model <- gge(dataUpdated, env, geno, yield)
l <- plot(gge_model,
          type = 6,
          col.gen = "black",
          col.env = "black",
          col.circle = "red",
          col.alpha.circle = 0.5,
          size.text.env = 4,
          axis_expand = 3,
          plot_theme = theme_metan(color.background = "white"))
arrange_ggplot(l)

#EXAMINE GENOTYPE
gge_model <- gge(dataUpdated, env, geno, yield, svp = "genotype")
n <- plot(gge_model,
          type = 7,
          sel_gen = "SB160",
          col.gen = "black",
          col.env = "black",
          size.text.env = 10,
          axis_expand = 1.5,
          plot_theme = theme_metan(grid = "both"))
arrange_ggplot(n)


#RANK GENOTYPES
gge_model = gge(dataUpdated,env,geno,yield, centering = "2", scaling ="sd", svp="1") 
p <- plot(gge_model,
          type = 8,
          col.gen = "orange",
          col.env = "blue",
          size.text.gen = 2,
          size.text.env = 2,
          axis_expand = 0.9,
          plot_theme = theme_metan(grid="both"))
arrange_ggplot(p)

#RELATIONSHIP AMONG ENV
gge_model <- gge(dataUpdated, env, geno, yield, centering = "2", scaling="1",svp="2")
t <- plot(gge_model,
          type = 10,
          col.gen = "black")
arrange_ggplot(t)


##REF https://tiagoolivoto.github.io/metan/articles/vignettes_gge.html
