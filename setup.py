from setuptools import setup

# https://python-packaging.readthedocs.io/en/latest/minimal.html
setup(
    author="Radian LLC",
    author_email="contact+ishikk@radian.codes",
    description="Backend module for ishikk.el, an Emacs calendar interface.",
    license="MIT",
    install_requires=["python-dateutil", "icalendar"],
    name="ishikk",
    url="https://github.com/radian-software/ishikk",
    version="1.0",
    zip_safe=True,
)
