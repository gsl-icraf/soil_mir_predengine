import glob
import re


def get_renv_excluded_files():
    r_files = glob.glob("**/*.R", recursive=True)
    r_files += glob.glob("**/*.r", recursive=True)
    qmd_files = glob.glob("**/*.qmd", recursive=True)
    all_files = r_files + qmd_files
    return all_files


def extract_package_from_line(line):
    """
    Extract package name from:
    library(pkg)
    library("pkg")
    library(pkg, quietly=TRUE)
    require(pkg)
    """
    match = re.search(r"(library|require)\(([^)]+)\)", line)
    # print(match)
    if match:
        inside = match.group(2)
        pkg = inside.split(",")[0].strip()
        pkg = pkg.replace('"', "").replace("'", "")
        return pkg
    return None


def get_packages(files):
    packages = set()
    for file in files:
        with open(file, "r") as f:
            for line in f:
                pkg = extract_package_from_line(line)
                if pkg:
                    packages.add(pkg)
    return list(packages)


def get_r_packages(file):
    """
    Extract package names only from:
    required-packages <- c(...)
    archived_packages <- c(...)
    """
    packages = set()
    tar_packages = set()
    github_installs = set()
    capture = False

    with open(file, "r") as f:
        for line in f:
            line = line.strip()

            if line.startswith("required_packages"):
                capture = True
                # print(line)
                continue

            if line.startswith("archived_packages"):
                capture = True
                # print(line)
                continue
            
            if line.startswith("github_installs"):
                capture = True
                # print(line)
                continue

            if capture:
                if ")" in line:
                    # print(line)
                    capture = False

                # Looks for values in between single and double quotes
                matches = re.findall(r"'([^']+)'|\"([^\"]+)\"", line)
                # print(matches)
                for m in matches:
                    value = m[0] or m[1]

                    # remove .tar.gz if archived
                    if value.endswith(".tar.gz"):
                        tar_packages.add(value.split("_")[0].strip())
                    elif "/" in value:
                        github_installs.add(value.split("/")[1].strip())
                    else:
                        packages.add(value)

    return list(packages.union(tar_packages, github_installs))



def check_packages(source_code_packages, pakages_r_list):
    missing = [pkg for pkg in source_code_packages if pkg not in pakages_r_list]

    if missing:
        raise ValueError(f"Missing in packages.R: {', '.join(sorted(missing))}")

    print("All packages have been included in packages.R")


source_code_files = get_renv_excluded_files()
source_code_packages = get_packages(source_code_files)
packages_r = get_r_packages("deployment/packages.R")

check_packages(source_code_packages, packages_r)
