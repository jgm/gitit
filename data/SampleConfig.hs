Config {
repositoryPath  = "wikidata",
userFile        = "gitit-users",
templateFile    = "template.html",
staticDir       = "static",
tableOfContents = False,
maxUploadSize   = 100000,
portNumber      = 5001,
passwordSalt    = "l91snthoae8eou2340987",
debugMode       = True,
frontPage       = "Front Page",
noEdit          = ["Help", "Front Page"],
noDelete        = ["Help", "Front Page"],
accessQuestion  = Just ("Enter the access code (to request an access code, contact me@somewhere.org):", ["abcd"])
}

