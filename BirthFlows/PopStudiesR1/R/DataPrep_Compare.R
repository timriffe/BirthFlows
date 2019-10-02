
# Compare B_1736to1750
library(here)
new <- readRDS(here("Data","ReduxTest","B_1736to1750_new.rds"))
old <- readRDS(here("Data","ReduxTest","B_1736to1750_old.rds"))

head(new)
head(old)
# these already different. Move up a step:

# masfr:
# same standard:
# new <- readRDS(here("Data","ReduxTest","masfr_new.rds"))
# old <- readRDS(here("Data","ReduxTest","masfr_old.rds"))
# old
# new
# plot(new$Age, new$masfr)
# lines(new$Age, old)

# 