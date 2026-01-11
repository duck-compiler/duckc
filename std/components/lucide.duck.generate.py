import os
import shutil
import subprocess
import re
import urllib.request

REPO_URL = "https://github.com/lucide-icons/lucide.git"
LICENSE_RAW_URL = "https://raw.githubusercontent.com/lucide-icons/lucide/main/LICENSE"
TEMP_DIR = "lucide_temp"
GEN_DIR = ".gen"
OUTPUT_FILE = "lucide.duck"
LICENSE_FILE = "lucide.duck.license.txt"

def kebab_to_pascal(text):
    return "".join(word.capitalize() for word in text.split("-"))

def setup():
    for path in [TEMP_DIR, GEN_DIR]:
        if os.path.exists(path):
            shutil.rmtree(path)

    if os.path.exists(OUTPUT_FILE):
        os.remove(OUTPUT_FILE)
    if os.path.exists(LICENSE_FILE):
        os.remove(LICENSE_FILE)

    os.makedirs(GEN_DIR)

def fetch_assets():
    subprocess.run([
        "git", "clone", "--depth", "1", "--filter=blob:none", "--sparse",
        REPO_URL, TEMP_DIR
    ], check=True, capture_output=True)

    subprocess.run(
        ["git", "sparse-checkout", "set", "icons"],
        cwd=TEMP_DIR, check=True, capture_output=True
    )

    license_temp_path = os.path.join(TEMP_DIR, "LICENSE")
    urllib.request.urlretrieve(LICENSE_RAW_URL, license_temp_path)

def generate():
    icons_src = os.path.join(TEMP_DIR, "icons")
    license_src = os.path.join(TEMP_DIR, "LICENSE")

    if not os.path.exists(icons_src):
        print("icins dir not found")
        return

    shutil.copy2(license_src, LICENSE_FILE)

    components = []
    svg_files = sorted([f for f in os.listdir(icons_src) if f.endswith(".svg")])

    print(f"processing {len(svg_files)} icons")
    for filename in svg_files:
        name_base = filename.replace(".svg", "")
        pascal_name = kebab_to_pascal(name_base)

        src_path = os.path.join(icons_src, filename)
        dest_path = os.path.join(GEN_DIR, filename)

        shutil.copy2(src_path, dest_path)

        with open(src_path, "r", encoding="utf-8") as f:
            svg_content = f.read().strip()
            svg_content = re.sub(r'<\?xml.*?\?>', '', svg_content).strip()

        component_code = (
            f"    component {pascal_name}(props: {{ className: String }}) jsx {{\n"
            f"        return (\n"
            f"            <span className={{props.className}}>\n"
            f"                {svg_content}\n"
            f"            </span>\n"
            f"        )\n"
            f"    }}\n"
        )
        components.append(component_code)

    with open(OUTPUT_FILE, "w", encoding="utf-8") as f:
        f.write("module lucide {\n")
        f.write("\n".join(components))
        f.write("}\n")

    print("generated lucide.duck and lucide.duck.license.txt")

if __name__ == "__main__":
    try:
        setup()
        fetch_assets()
        generate()
    finally:
        if os.path.exists(TEMP_DIR):
            shutil.rmtree(TEMP_DIR)
        if os.path.exists(GEN_DIR):
            shutil.rmtree(GEN_DIR)
