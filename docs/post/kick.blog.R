library(blogdown)
install_hugo(force=T)
#new_site("calintat/minimal") 

build_site()
serve_site()

blogdown::new_post("CRM_trialr_stan")
#blogdown::new_post("research_assistant")
#blogdown::new_post("JACBT_symposium_information")
