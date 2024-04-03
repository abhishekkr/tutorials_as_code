from selenium import webdriver
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.chrome.service import Service
import sys
import os


CHROMEDRIVER_PATH = os.getenv('CHROMEDRIVER_PATH')
CHROME_PATH = os.getenv('CHROME_HEADLESS_PATH')
WINDOW_SIZE = "1920,1080"


def get_browser():
    svc = Service(executable_path=CHROMEDRIVER_PATH)
    opts = Options()
    opts.add_argument("--window-size=%s" % WINDOW_SIZE)
    opts.binary_location = CHROME_PATH
    return webdriver.Chrome(service=svc, options=opts)


def get_page_screenshot(url, screenshot_img):
    browser = get_browser()
    browser.get(url)
    browser.get_screenshot_as_file(screenshot_img)
    browser.close()


if __name__ == "__main__":
    url = sys.argv[1]
    img = sys.argv[2]
    get_page_screenshot(url, img)
