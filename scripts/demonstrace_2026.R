xfun::pkg_attach2("tidyverse", # důležité jsou hlavně balíčky purrr a ggplot2
                  "terra",
                  "RCzechia", # načítá i balíček sf
                  "fs", # balíček pro správu složek apod.
                  message = F) # můžeme ztišit startovní zprávy, včetně upozornění na konflikty

soubory <- dir("geodata",
               pattern = "\\.tif$",
               full.names = T)

r <- rast(soubory)

r

?tapp

?SPEI::thornthwaite

# stáhneme polygon, podle kterého se omezíme na dané území
hranice <- republika()

rcz <- r |> 
  crop(hranice, # není třeba transformovat, crs jsou stejné
       mask = T)

g <- expand.grid(1961:1990,
                 rep(1:12,
                     each = 30)) |> 
  as.data.frame() |> 
  as_tibble() |> 
  arrange(Var1,
          Var2) |> 
  group_by(Var1,
           Var2) |> 
  mutate(id = cur_group_id())

g |> 
  tail()

rcz_mesice <- rcz |> 
  tapp(index = g$id,
       fun = "mean")

nlyr(rcz_mesice)

plot(rcz_mesice[[7]])

rcz_mesice_celsius <- (rcz_mesice - 273.15) |> 
  round(1)

plot(rcz_mesice_celsius[[7]])

# ještě si pro úplnost vrstvy pojmenujme a přidělme informaci o datumu
names(rcz_mesice_celsius) <- str_c("ym_",
                                   format(seq(ym(196101),
                                              ym(199012),
                                              "month"),
                                          "%Y%m"))

time(rcz_mesice_celsius) <- seq(ym(196101),
                                ym(199012),
                                "month")

# co vzniklo?
rcz_mesice_celsius

?lapp

s <- sds(rcz_mesice_celsius,
         init(rcz_mesice_celsius, # funkcí terra::init() získáme snadno novou vrstvu zeměpisných šířek
              fun = "y"))

tictoc::tic(msg = "čas strávený odvozením PET") # tictoc musí být nainstalovaný
pet <- lapp(s,
            fun = \(x, y) {
              if (all(is.na(x))) {return(rep(NA, # podmínku musíme nastavit, jinak jsou vráceny nuly i pro místa, kde výpočet nemůže proběhnout
                                             times = length(x)))
              }
              else {SPEI::thornthwaite(Tave = x, # SPEI musí být nainstalovaný
                                       lat = y,
                                       na.rm = T, # nastavujeme kvůli hláškám, ostatních hlášek si nevšímáme
                                       verbose = F)
              }
            })
tictoc::toc()

# ještě poslední úpravy
pet <- pet |> 
  round(1)

names(pet) <- names(rcz_mesice_celsius)

time(pet) <- time(rcz_mesice_celsius)

# vykreslíme opět červenec 1961
plot(pet[[7]])

# nastavme vektor cest a názvů souborů
# podsložku vystup1 máme vytvořenou předem

if (!dir_exists("geodata/vystup1")) dir_create("geodata/vystup1") # tvoříme podsložku, pokud neexistuje

nazvy <- str_glue("geodata/vystup1/pet_{names(pet)}.tif")

# uložme vrstvy zvlášť nativním způsobem
tictoc::tic(msg = "čas strávený nativním způsobem ukládání souborů")
writeRaster(pet,
            nazvy,
            overwrite = T) # pro případ, že bychom už některé soubory dříve vytvořili a chtěli je přepsat
tictoc::toc()