# 🌐Lecture des feuillets deja detectees sur Github 🌐
GET("https://api.github.com/repos/FraLessard/parois/contents/parois") %>% 
  content() %>% 
  map_dfr(~tibble(name = .x$name,
                  download_url = .x$download_url)) %>% 
  mutate(feuillet = pull(.,name) %>% gsub("\\..*$", "", .) %>% gsub("parois_", "", .)) %>% 
  mutate(dest = paste0("./parois/", name)) -> feuillets_detectes_github