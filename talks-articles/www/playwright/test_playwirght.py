import re
from playwright.sync_api import Page, expect

def test_has_title(page: Page):
    page.goto("https://playwright.dev/")
    expect(page).to_have_title(re.compile("Playwright"))

def test_get_started_link(page: Page):
    page.goto("https://playwright.dev/")
    page.get_by_role("link", name="Get started").click()
    expect(page.get_by_role("heading", name="Installation")).to_be_visible()

def test_get_lv_link(page: Page):
    page.goto("https://playwright.dev/")
    page.get_by_role("link", name="Learn Videos").click()
    #expect(page.get_by_role("paragraph", name="Check out the latest videos for learning Playwright")).to_be_visible()
    expect(page.get_by_text("Check out the latest videos for learning Playwright")).to_be_visible()
