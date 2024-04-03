#!bash

if [[ -d "./.venv" ]]; then
  source .venv/bin/activate
else
  echo "Preparing Virtualenv"
  python -m venv .venv && \
  source .venv/bin/activate
  pip install selenium
fi


export CHROME_HEADLESS_PATH="/tmp/selenium/chrome-headless-shell-linux64/chrome-headless-shell"
export CHROMEDRIVER_PATH="/tmp/selenium/chromedriver-linux64/chromedriver"
export PATH="/tmp/selenium/chrome-headless-shell-linux64:/tmp/selenium/chromedriver-linux64:${PATH}"


if [[ ! -f "${CHROME_HEADLESS_PATH}" ]]; then
  [[ `uname -s` != "Linux" ]] && \
    echo "[ERROR] Missing Chrome-Headless. Make it available at: ${CHROME_HEADLESS_PATH}" && \
    exit 123
  echo "Preparing Chrome-Headless"
  mkdir -p "/tmp/selenium"
  set -e
  pushd "/tmp/selenium"
  curl -LkO "https://storage.googleapis.com/chrome-for-testing-public/123.0.6312.86/linux64/chrome-headless-shell-linux64.zip"
  unzip "chrome-headless-shell-linux64.zip"
  rm "chrome-headless-shell-linux64.zip"
  popd
  set +e
fi

if [[ ! -f "${CHROMEDRIVER_PATH}" ]]; then
  [[ `uname -s` != "Linux" ]] && \
    echo "[ERROR] Missing ChromeDriver. Make it available at: ${CHROMEDRIVER_PATH}" && \
    exit 123
  echo "Preparing ChromeDriver"
  mkdir -p "/tmp/selenium"
  set -e
  pushd "/tmp/selenium"
  curl -LkO "https://storage.googleapis.com/chrome-for-testing-public/123.0.6312.86/linux64/chromedriver-linux64.zip"
  unzip "chromedriver-linux64.zip"
  rm "chromedriver-linux64.zip"
  popd
  set +e
fi

echo "Running Selenium script.."
SOME_URL="https://selenium.dev"
SOME_IMG="/tmp/selenium-webpage.png"
python ./selenium-headless-sample.py "${SOME_URL}" "${SOME_IMG}"

echo "open: ${SOME_IMG}"

deactivate
