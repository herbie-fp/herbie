import json, html, sys

REPORTS_PATH = sys.argv[1]
HERBIE20_REPORT_PATH = REPORTS_PATH + "/herbie20/results.json"
VANILLA_REPORT_PATH = REPORTS_PATH + "/no-accelerators/results.json"
GROW_REPORT_PATH = REPORTS_PATH + "/grow/results.json"
OUTPUT_PATH = REPORTS_PATH + "/index.html"
HERBIE20_FOLDER = "herbie20"
GROW_FOLDER = "grow"
VANILLA_FOLDER ="no-accelerators"
ACCELERATORS = ["sin-xy", "cos-xy", "cos-quotient-xy", "sin-quotient-xy"]

def calculate_end_accuracy(data):
    total_end = 0
    max_accuracy = 0
    for test in data.get("tests", []):
        total_end += test.get("end", 0)
        max_accuracy += test.get("bits", 0)
    if max_accuracy == 0:
        return 0.0
    return 100 - (100 * (total_end / max_accuracy))

def load_report(path):
    with open(path, 'r') as f:
        return json.load(f)

def get_accuracy_str(path):
    report = load_report(path)
    percent = calculate_end_accuracy(report)
    return f"{percent:.1f}%"


def format_accelerator_rows(accelerators, counts):
    rows = []
    for acc in accelerators:
        count = counts.get(acc, 0)
        rows.append(f'<tr><td>{acc}</td><td>{count}</td></tr>')
    return "".join(rows)

def read_text(path):
    with open(path, 'r') as f:
        return f.read()

herbie20_accuracy_str = get_accuracy_str(HERBIE20_REPORT_PATH)
vanilla_accuracy_str = get_accuracy_str(VANILLA_REPORT_PATH)
grow_accuracy_str = get_accuracy_str(GROW_REPORT_PATH)

counts = {
    acc_name: read_text(GROW_REPORT_PATH).count(acc_name)
    for acc_name in ACCELERATORS
    if acc_name
}

accelerator_rows = format_accelerator_rows(ACCELERATORS, counts)

html_content = f"""
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <title>Comparing Platforms</title>
</head>
<body>    
    <h2>Overall Summary</h2>
    <p><strong>No Accelerators:</strong> {vanilla_accuracy_str} - <a href="{VANILLA_FOLDER}/">Folder</a></p>
    <p><strong>Regular Herbie:</strong> {herbie20_accuracy_str} - <a href="{HERBIE20_FOLDER}/">Folder</a></p>
    <p><strong>Grow:</strong> {grow_accuracy_str} - <a href="{GROW_FOLDER}/">Folder</a></p>
    
    <h2>Accelerators</h2>
    <table border="1" style="border-collapse:collapse; width:100%; max-width:900px;">
        <tr>
            <th>Name</th>
            <th>Uses</th>
        </tr>
        {accelerator_rows}
    </table>
    

</body>
</html>
"""

with open(OUTPUT_PATH, 'w') as f:
    f.write(html_content)
