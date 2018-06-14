from setuptools import setup

# https://python-packaging.readthedocs.io/en/latest/minimal.html
setup(
    author="Radon Rosborough",
    author_email="radon.neon@gmail.com",
    description="Backend module for ishikk.el, an Emacs calendar interface.",
    license="MIT",
    install_requires=["python-dateutil", "icalendar"],
    name="ishikk",
    url="https://github.com/raxod502/ishikk",
    version="1.0",
    zip_safe=True,
)
