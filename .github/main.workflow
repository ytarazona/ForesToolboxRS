workflow "Render and Deploy RMarkdown Website" {
  on = "push"
  resolves = [
    "Render", 
    "Deploy"
  ]
}

action "Build image" {

  uses = "actions/docker/cli@aea64bb1b97c42fa69b90523667fef56b90d7cff"
  args = [
    "build --tag=repo:latest ."
  ]
}

action "Render" {
  needs = [
    "Build image"
  ]
  uses = "maxheld83/ghactions/Rscript-byod@master"
  args = [
    "--verbose", 
    "--echo", 
    "-e \"{\"", 
    "-e \"    deploy_dir <- rmarkdown::render_site(encoding = \"UTF-8\")\"", 
    "-e \"    readr::write_lines(x = deploy_dir, path = \".deploy_dir\", \"", 
    "-e \"        append = FALSE)\"", 
    "-e \"}\""
  ]
}

action "Filter master" {
  needs = [
    "Render"
  ]
  uses = "actions/bin/filter@a9036ccda9df39c6ca7e1057bc4ef93709adca5f"
  args = [
    "branch master"
  ]
}

action "Deploy" {
  needs = [
    "Filter master"
  ]
  uses = "maxheld83/ghpages@v0.2.0"
  env = {
    BUILD_DIR = "_site"
  }
  secrets = [
    "GH_PAT"
  ]
}

