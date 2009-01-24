Config {
repository          = Git "wikidata",
defaultPageType     = Markdown,
userFile            = "gitit-users",
templateFile        = "template.html",
staticDir           = "static",
tableOfContents     = False,
maxUploadSize       = 100000,
portNumber          = 5001,
debugMode           = True,
frontPage           = "Front Page",
noEdit              = ["Help", "Front Page"],
noDelete            = ["Help", "Front Page"],
accessQuestion      = Just ("Enter the access code (to request an access code, contact me@somewhere.org):", ["abcd"]),
useRecaptcha        = False,
recaptchaPublicKey  = "",
recaptchaPrivateKey = "",
mimeTypesFile       = "/etc/mime.types"
}

