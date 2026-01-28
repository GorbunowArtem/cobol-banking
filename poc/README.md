# 1. Install GnuCOBOL

sudo apt update && sudo apt install gnucobol4 mysql-client -y

# 2. Set envs

export MYSQL_HOST=""
export MYSQL_USER=""
export MYSQL_PASS=""
export MYSQL_DB=""
export MYSQL_DB_SOURCE=""
export MYSQL_DB_TARGET=""

# 3. Run app demo script

./demo-journey.sh
