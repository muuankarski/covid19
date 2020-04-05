  #!/usr/bin/r
setwd("/home/aurelius/serveri/covid19/")

system('rsync -avzhe "ssh -i /home/aurelius/avaimet/nuc-rsync-key" --delete --progress --exclude=.Rproj.user/ --exclude=.git /home/aurelius/serveri/covid19/ muuankarski@kapsi.fi:sites/data.markuskainu.fi/www/covid19_test/')
