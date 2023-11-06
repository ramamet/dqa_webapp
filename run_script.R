# -----
# Ref
# https://www.rostrum.blog/posts/2023-10-08-govspeakify-tables/


# Take the Shiny app and assets from the penguins_shiny folder and generate a deployable version of it in a folder called docs/
shinylive::export("app", "docs")

# To launch the app in a local static server
httpuv::runStaticServer("docs")

#?
httpuv::stopAllServers()
