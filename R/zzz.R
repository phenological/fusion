message(cat(crayon::yellow("'fusion' versions > 0.0.1 introduced breaking changes.")))
message(cat(crayon::yellow("Use release 0.0.1 if not sure.")))
message(cat(crayon::yellow("remotes::install_github('phenological/fusion@0.0.1')")))

ipkgs <- installed.packages()
# ipkgs[grep("fusion", ipkgs[,1]), c(1,3)]
# ipkgs[grep("nmr.parser", ipkgs[,1]), c(1,3)]
rldx_check <- ipkgs[grep("rldx", ipkgs[,1]), c(1,3)]
# utils::packageVersion("fusion")
# utils::packageVersion("rldx")

if (length(rldx_check) == 2) {
  message(cat(crayon::yellow("You are currently using rldx version:", rldx_check[2])))
} else {
  message(cat(crayon::yellow("Try installing rldx package using: \n")))
  message(cat(crayon::yellow("remotes::install_url('https://134.115.48.123/gitea/jul/rldx/archive/main.tar.gz')")))
  message(cat(crayon::blue("use this to avoid certificate check\n")))
  message(cat(crayon::blue("set_config(config(ssl_verifypeer = 0L, ssl_verifyhost = 0L))\n")))
  message(cat(crayon::blue("options(download.file.method=\"curl\", download.file.extra=\"-k -L\")\n")))
  # remotes::install_url("https://134.115.48.123/gitea/jul/rldx/archive/main.tar.gz")
  # ipkgs <- installed.packages()
  # rldx_check <- ipkgs[grep("rldx", ipkgs[,1]), c(1,3)]
  # message(cat(crayon::yellow("rldx version:", rldx_check[2], "has been installed.")))
}
