library(blogdown)
install_hugo(force=T)
#new_site("calintat/minimal") 

build_site()
serve_site()

new_content("post/IASC_positive_side.Rmd")
#blogdown::new_post("research_assistant")
#blogdown::new_post("JACBT_symposium_information")
