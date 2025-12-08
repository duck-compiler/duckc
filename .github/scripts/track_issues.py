import os
import re
import hashlib
import json
import subprocess
import requests
from datetime import datetime

TARGET_DIRECTORY = os.environ.get("SCAN_TARGET_DIR", "../../src")
STATE_DIR = "./.issues/state"
ISSUES_DIR = "./.issues"
FILE_EXTENSIONS = (".rs",)
COMMENT_MARKER = "//"
COMMENT_TAGS = ("todo", "review")
GITHUB_REPO_URL = "https://github.com/duck-compiler/duckc"

MODULE_TAG_COLORS = [
    "#d73a49", "#6f42c1", "#007bff", "#28a745", "#ffc107", "#17a2b8",
    "#e83e8c", "#fd7e14", "#20c997", "#6610f2", "#0366d6"
]

def get_color_for_tag(tag_name):
    if not tag_name:
        return "#6c757d"
    hash_val = int(hashlib.md5(tag_name.encode()).hexdigest(), 16)
    return MODULE_TAG_COLORS[hash_val % len(MODULE_TAG_COLORS)]

def get_text_color_for_bg(hex_color):
    hex_color = hex_color.lstrip('#')
    r, g, b = (int(hex_color[i:i+2], 16) for i in (0, 2, 4))
    luminance = (0.299 * r + 0.587 * g + 0.114 * b) / 255
    return '#ffffff' if luminance < 0.5 else '#212529'

AVATAR_CACHE = {}
def get_github_user_info(email=None, username=None):
    """
    Fetches GitHub user info (avatar, profile URL, name) by email or username.
    Caches results to avoid redundant API calls.
    Returns a dictionary with user info.
    """
    default_info = {
        "avatar_url": "https://placehold.co/40x40/cccccc/ffffff?text=?",
        "html_url": "#",
        "login": "Unknown",
        "name": "Unknown"
    }

    if username:
        cache_key = f"user:{username}"
        api_url = f"https://api.github.com/users/{username.lstrip('@')}"
    elif email:
        cache_key = f"email:{email}"
        api_url = f"https://api.github.com/search/users?q={email}+in:email"
    else:
        return default_info

    if cache_key in AVATAR_CACHE:
        return AVATAR_CACHE[cache_key]

    try:
        response = requests.get(api_url, timeout=5)
        response.raise_for_status()
        data = response.json()

        user_data = {}
        if username:
            user_data = data
        elif data.get("total_count", 0) > 0:
            user_data = data["items"][0]

        if not user_data:
            AVATAR_CACHE[cache_key] = default_info
            return default_info

        name = user_data.get("name") or user_data.get("login")
        info = {
            "avatar_url": f"{user_data.get('avatar_url')}&s=40" if user_data.get('avatar_url') else default_info['avatar_url'],
            "html_url": user_data.get('html_url', '#'),
            "login": user_data.get('login', 'Unknown'),
            "name": name or "Unknown"
        }
        AVATAR_CACHE[cache_key] = info
        return info

    except requests.RequestException as e:
        print(f"Warning: Could not fetch GitHub data for {cache_key}: {e}")

    AVATAR_CACHE[cache_key] = default_info
    return default_info

def get_git_info():
    """
    Gets the git repository root and current branch.
    """
    try:
        root = subprocess.run(['git', 'rev-parse', '--show-toplevel'], capture_output=True, text=True, check=True, encoding='utf-8').stdout.strip()
        branch = subprocess.run(['git', 'rev-parse', '--abbrev-ref', 'HEAD'], capture_output=True, text=True, check=True, encoding='utf-8').stdout.strip()
        return {'root': root, 'branch': branch}
    except (subprocess.CalledProcessError, FileNotFoundError) as e:
        print(f"Warning: Could not get git info. Is this a git repository? Error: {e}")
        return None

def get_git_blame_info(file_path, line_number):
    try:
        command = ["git", "blame", "-L", f"{line_number},{line_number}", "--porcelain", file_path]
        result = subprocess.run(command, capture_output=True, text=True, check=True, encoding='utf-8')

        author, author_mail = None, None
        for line in result.stdout.splitlines():
            if line.startswith("author "):
                author = line.split(" ", 1)[1]
            elif line.startswith("author-mail "):
                author_mail = line.split(" ", 1)[1].strip("<>")

        if author and author_mail:
            return {"name": author, "email": author_mail}

    except (subprocess.CalledProcessError, FileNotFoundError):
        pass
    return None

def parse_line_for_issue(line):
    """
    Parses a line for an issue, handling both comment tags and todo! macros.
    """
    stripped_line = line.strip()

    macro_match = re.match(r'^\s*todo!\s*\(\s*"(.*?)"\s*\)', stripped_line)
    if macro_match:
        return {
            "tag": "todo",
            "user": None,
            "issue_ref": None,
            "title": macro_match.group(1) or "Untitled todo macro"
        }

    if re.match(r'^\s*todo!\s*\(\s*\)', stripped_line):
        return {
            "tag": "todo",
            "user": None,
            "issue_ref": None,
            "title": stripped_line
        }

    if stripped_line.startswith(COMMENT_MARKER):
        content = stripped_line[len(COMMENT_MARKER):].strip()
        pattern = re.compile(
            r"^\s*(" + "|".join(COMMENT_TAGS) + r")(?:\((@[\w\d_-]+)?(?:,\s*)?(#[\w\d_-]+)?\))?[:\s]*(.*)", re.IGNORECASE
        )
        comment_match = pattern.match(content)
        if comment_match:
            tag, user, issue_ref, title = comment_match.groups()
            return {
                "tag": tag.lower().strip(),
                "user": user.strip() if user else None,
                "issue_ref": issue_ref.strip() if issue_ref else None,
                "title": title.strip()
            }

    return None

def generate_issue_id(file_path, title):
    normalized_path = os.path.normpath(file_path)
    unique_string = f"{normalized_path}:{title}"
    return hashlib.md5(unique_string.encode('utf-8')).hexdigest()

def find_issues_in_file(file_path, scan_dir):
    issues_found = []

    try:
        relative_path = os.path.relpath(file_path, scan_dir)
        path_parts = relative_path.split(os.path.sep)
        module_tag = path_parts[0] if len(path_parts) > 1 else "root"
    except ValueError:
        module_tag = os.path.basename(os.path.dirname(file_path))

    try:
        with open(file_path, 'r', encoding='utf-8') as f:
            lines = f.readlines()
    except Exception as e:
        print(f"Warning: Could not read file {file_path}: {e}")
        return []

    for i, line in enumerate(lines):
        parsed_header = parse_line_for_issue(line)
        if parsed_header and parsed_header["title"]:
            line_number = i + 1
            is_macro_todo = line.strip().startswith('todo!')

            if is_macro_todo:
                filename_without_path = os.path.basename(file_path)
                final_title = f"{line.strip()} in {filename_without_path} @ line {line_number}"
            else:
                final_title = parsed_header["title"]

            description_lines = []
            if not is_macro_todo and line.strip().startswith(COMMENT_MARKER):
                for j in range(i + 1, len(lines)):
                    next_line_content = lines[j].strip()
                    if next_line_content.startswith(COMMENT_MARKER):
                        description_lines.append(next_line_content[len(COMMENT_MARKER):].strip())
                    else:
                        break

            description = "\n".join(description_lines)
            issue_id = generate_issue_id(file_path, parsed_header["title"])
            blame_info = get_git_blame_info(file_path, line_number)

            issues_found.append({
                "id": issue_id, "file_path": file_path, "line_number": line_number,
                "title": final_title, "tag": parsed_header["tag"],
                "module_tag": module_tag, "user": parsed_header["user"],
                "issue_ref": parsed_header["issue_ref"], "description": description,
                "blame": blame_info, "source_line": line.strip()
            })
    return issues_found

def get_all_source_files(directory):
    source_files = []
    for root, _, files in os.walk(directory):
        for file in files:
            if file.endswith(FILE_EXTENSIONS):
                source_files.append(os.path.join(root, file))

    print(source_files)
    return source_files

def update_issue_database(found_issues):
    if not os.path.exists(STATE_DIR):
        os.makedirs(STATE_DIR)

    activities = []
    now_iso = datetime.now().isoformat()

    tracked_issue_files = {f.replace('.json', '') for f in os.listdir(STATE_DIR) if f.endswith('.json') and f != 'activity_log.json'}
    found_issue_ids = set()

    for issue in found_issues:
        issue_id = issue["id"]
        found_issue_ids.add(issue_id)
        issue_file_path = os.path.join(STATE_DIR, f"{issue_id}.json")

        if os.path.exists(issue_file_path):
            with open(issue_file_path, 'r', encoding='utf-8') as f:
                existing_data = json.load(f)

            if existing_data.get("description") != issue.get("description"):
                if "history" not in existing_data:
                    existing_data["history"] = []
                existing_data["history"].append({
                    "timestamp": datetime.now().isoformat(),
                    "description": existing_data.get("description")
                })
                activities.append({
                    "type": "modified", "timestamp": now_iso, "issue_id": issue_id,
                    "issue_title": issue['title'], "author_info": issue.get('blame')
                })

            issue_data = {**existing_data, **issue, "status": "open"}
        else:
            issue_data = {**issue, "status": "open", "first_seen": datetime.now().isoformat(), "history": []}
            activities.append({
                "type": "created", "timestamp": now_iso, "issue_id": issue_id,
                "issue_title": issue['title'], "author_info": issue.get('blame')
            })

        with open(issue_file_path, 'w', encoding='utf-8') as f:
            json.dump(issue_data, f, indent=4)

    closed_issue_ids = tracked_issue_files - found_issue_ids
    for issue_id in closed_issue_ids:
        issue_file_path = os.path.join(STATE_DIR, f"{issue_id}.json")
        with open(issue_file_path, 'r', encoding='utf-8') as f:
            issue_data = json.load(f)
        if issue_data["status"] == "open":
            issue_data["status"] = "closed"
            issue_data["closed_at"] = datetime.now().isoformat()
            activities.append({
                "type": "closed", "timestamp": now_iso, "issue_id": issue_id,
                "issue_title": issue_data['title'], "author_info": issue_data.get('blame')
            })
        with open(issue_file_path, 'w', encoding='utf-8') as f:
            json.dump(issue_data, f, indent=4)

    log_path = os.path.join(STATE_DIR, "activity_log.json")
    all_activities = []
    if os.path.exists(log_path):
        with open(log_path, 'r', encoding='utf-8') as f:
            all_activities = json.load(f)

    all_activities = activities + all_activities
    with open(log_path, 'w', encoding='utf-8') as f:
        json.dump(all_activities[:100], f, indent=4)

    return len(found_issues), len(closed_issue_ids)

def render_issue_table_html(issues, git_info):
    """Helper function to render a table of issues."""
    if not issues:
        return '<tr><td colspan="3" style="text-align:center; padding: 2rem;">No issues in this category.</td></tr>'

    table_html = ""
    for issue in issues:
        module_tag_name = issue.get('module_tag', 'general')
        module_bg_color = get_color_for_tag(module_tag_name)
        module_text_color = get_text_color_for_bg(module_bg_color)

        author_info = get_github_user_info(email=issue.get('blame', {}).get('email'))
        author_avatar_html = f'<a href="{author_info["html_url"]}" target="_blank" title="{author_info.get("name", author_info.get("login"))}"><img src="{author_info["avatar_url"]}" class="avatar" onerror="this.src=\'https://placehold.co/40x40/cccccc/ffffff?text=?\'; this.onerror=null;"></a>'

        assignee_avatar_html = ""
        if issue.get('user'):
            assignee_info = get_github_user_info(username=issue.get('user'))
            assignee_avatar_html = f'<span class="arrow">→</span><a href="{assignee_info["html_url"]}" target="_blank" title="{assignee_info.get("name", assignee_info.get("login"))}"><img src="{assignee_info["avatar_url"]}" class="avatar" onerror="this.src=\'https://placehold.co/40x40/cccccc/ffffff?text=?\'; this.onerror=null;"></a>'

        summary_html = f"""
            <tr class="issue-summary" onclick="toggleDetails(this)">
                <td width="200">
                    <span class="tag">{issue['tag']}</span>
                    <span class="tag module-tag" style="background-color:{module_bg_color}; color:{module_text_color};">{module_tag_name}</span>
                </td>
                <td class="title">{issue['title']}</td>
                <td width="120" align="right">
                    <div class="avatar-stack">{author_avatar_html}{assignee_avatar_html}</div>
                </td>
            </tr>
        """

        author_html = f'<div class="author-block">{author_avatar_html}<span>{issue.get("blame", {}).get("name", "Unknown")}</span></div>'

        description_html = f"<pre class='description-pre'>{issue['description']}</pre>" if issue.get('description') else "<p>No description provided.</p>"
        if issue.get('history'):
            history_html = ""
            for old_version in reversed(issue['history']):
                ts = datetime.fromisoformat(old_version['timestamp']).strftime('%Y-%m-%d %H:%M')
                history_html += f"<strong>Changed on {ts}:</strong><pre class='description-pre'>{old_version['description']}</pre>"
            description_html += f"<details><summary>View description history</summary>{history_html}</details>"

        if git_info:
            try:
                source_branch = os.environ.get("SOURCE_BRANCH", git_info.get('branch', 'main'))
                source_prefix = os.environ.get("SOURCE_PATH_PREFIX", git_info.get('root'))

                url_file_path = os.path.relpath(issue['file_path'], source_prefix).replace(os.path.sep, '/')

                github_url = f"{GITHUB_REPO_URL}/blob/{source_branch}/{url_file_path}#L{issue['line_number']}"
                github_link_html = f'<a href="{github_url}" target="_blank">View on GitHub</a>'
            except (ValueError, TypeError):
                github_link_html = "<span>Path error</span>"
        else:
            github_link_html = "<span></span>"

        display_file_path = issue['file_path'].lstrip('./').lstrip('../')

        details_html = f"""
            <tr class="issue-details"><td colspan="3">
                <div class="issue-details-content"><div class="details-grid">
                    <div class="details-box">
                        <h3>Details</h3>
                        <strong>Author:</strong> {author_html}<br>
                        <strong>File:</strong> <span class="file-path">{display_file_path}</span><br>
                        <strong>Location:</strong> {github_link_html}<br>
                        <strong>First Seen:</strong> {datetime.fromisoformat(issue['first_seen']).strftime('%Y-%m-%d')}
                        <h4 class="code-header">Code</h4>
                        <pre class="code-line">{issue.get('source_line', 'N/A')}</pre>
                    </div>
                    <div class="details-box"><h3>Description</h3>{description_html}</div>
                </div></div>
            </td></tr>
        """
        table_html += summary_html + details_html
    return table_html

def generate_html_report(git_info):
    """
    Generates a collapsible, tabbed HTML report with an activity log.
    """
    report_path = os.path.join(ISSUES_DIR, "issues.html")
    all_issues = []
    if os.path.exists(STATE_DIR):
        for filename in sorted(os.listdir(STATE_DIR)):
            if filename.endswith(".json") and filename != "activity_log.json":
                with open(os.path.join(STATE_DIR, filename), 'r', encoding='utf-8') as f:
                    all_issues.append(json.load(f))

    open_issues = sorted([i for i in all_issues if i['status'] == 'open'], key=lambda x: (x.get('module_tag', 'zzzz'), x['file_path']))
    closed_issues = sorted([i for i in all_issues if i['status'] == 'closed'], key=lambda x: (x.get('module_tag', 'zzzz'), x.get('closed_at', ''), x['file_path']))

    open_issues_html = render_issue_table_html(open_issues, git_info)
    closed_issues_html = render_issue_table_html(closed_issues, git_info)

    activity_log_html = ""
    log_path = os.path.join(STATE_DIR, "activity_log.json")
    if os.path.exists(log_path):
        with open(log_path, 'r', encoding='utf-8') as f:
            activities = json.load(f)

        for activity in activities:
            author_info = get_github_user_info(email=activity.get('author_info', {}).get('email'))
            author_avatar = f'<a href="{author_info["html_url"]}" target="_blank" title="{author_info.get("name", author_info.get("login"))}"><img src="{author_info["avatar_url"]}" class="avatar-small" onerror="this.src=\'https://placehold.co/40x40/cccccc/ffffff?text=?\'; this.onerror=null;"></a>'

            ts = datetime.fromisoformat(activity['timestamp']).strftime('%b %d, %H:%M')

            if activity['type'] == 'created':
                verb = "created"
                icon = "➕"
            elif activity['type'] == 'closed':
                verb = "closed"
                icon = "✔️"
            else:
                verb = "modified"
                icon = "✏️"

            activity_log_html += f"""
                <div class="activity-item">
                    <div class="activity-icon">{icon}</div>
                    <div class="activity-details">
                        {author_avatar} <strong>{author_info['name']}</strong> {verb} issue:<br>
                        <em>{activity['issue_title']}</em><br>
                        <span class="activity-time">{ts}</span>
                    </div>
                </div>
            """

    branch_info_html = ""
    if git_info and git_info.get('branch'):
        branch_info_html = f"<p>Generated on branch: <strong>{git_info['branch']}</strong></p>"

    html_content = f"""
    <!DOCTYPE html>
    <html lang="en">
    <head>
        <meta charset="UTF-8"><meta name="viewport" content="width=device-width, initial-scale=1.0">
        <title>🦆 Duck Issue Tracker</title>
        <style>
            body {{ font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Helvetica, Arial, sans-serif; line-height: 1.6; background-color: #f8f9fa; color: #212529; margin: 0; padding: 2rem; }}
            .container {{ max-width: 1600px; margin: auto; }}
            .page-grid {{ display: grid; grid-template-columns: 3fr 1fr; gap: 2rem; }}
            .main-content {{ background: #fff; padding: 2rem; border-radius: 8px; box-shadow: 0 4px 6px rgba(0,0,0,0.1); }}
            .activity-log-container {{ background: #fff; padding: 1.5rem; border-radius: 8px; box-shadow: 0 4px 6px rgba(0,0,0,0.1); height: fit-content; max-height: 90vh; overflow-y: auto; }}
            h1, h2 {{ color: #343a40; border-bottom: 2px solid #dee2e6; padding-bottom: 0.5rem; }}
            .tab-buttons {{ border-bottom: 2px solid #dee2e6; margin-bottom: 1.5rem; }}
            .tab-button {{ background: none; border: none; padding: 1rem 1.5rem; font-size: 1rem; cursor: pointer; }}
            .tab-button.active {{ border-bottom: 3px solid #007bff; font-weight: bold; color: #007bff; }}
            .tab-content {{ display: none; }}
            .tab-content.active {{ display: block; }}
            .issue-table {{ width: 100%; border-collapse: collapse; }}
            .issue-summary td {{ padding: 0.8rem 1rem; vertical-align: middle; }}
            .issue-summary {{ cursor: pointer; border-bottom: 1px solid #dee2e6; }}
            .issue-summary:hover {{ background-color: #f1f3f5; }}
            .issue-details {{ display: none; }}
            .issue-details-content {{ background-color: #f8f9fa; padding: 1.5rem; }}
            .tag {{ padding: 0.2rem 0.6rem; border-radius: 4px; font-size: 0.85rem; margin-right: 5px; background-color: #6c757d; color: white; }}
            .title {{ font-weight: 500; }}
            .description-pre, .code-line {{ white-space: pre-wrap; word-wrap: break-word; font-family: "SF Mono", "Fira Code", "Consolas", monospace; margin-top: 0.5rem; padding: 1rem; border-radius: 4px; }}
            .description-pre {{ background: #e9ecef; }}
            .code-line {{ background: #fffbdd; border: 1px solid #ffeeba; }}
            .code-header {{ margin-top: 1rem; margin-bottom: 0.5rem; color: #495057; }}
            .footer {{ margin-top: 2rem; text-align: center; color: #6c757d; font-size: 0.9rem; }}
            .avatar, .avatar-small {{ border-radius: 50%; vertical-align: middle; }}
            .avatar {{ width: 32px; height: 32px; }}
            .avatar-small {{ width: 24px; height: 24px; }}
            .avatar-stack {{ display: flex; align-items: center; justify-content: flex-end; gap: 5px; }}
            .arrow {{ font-size: 1.2rem; color: #6c757d; }}
            .details-grid {{ display: grid; grid-template-columns: 1fr 1fr; gap: 1.5rem; }}
            .details-box h3 {{ margin-top: 0; color: #495057; border-bottom: 1px solid #dee2e6; padding-bottom: 0.5rem; }}
            .author-block {{ display: flex; align-items: center; gap: 10px; }}
            .file-path {{ font-family: "SF Mono", "Fira Code", "Consolas", monospace; }}
            a {{ color: #007bff; text-decoration: none; }}
            a:hover {{ text-decoration: underline; }}
            .activity-item {{ display: flex; gap: 1rem; padding-bottom: 1rem; margin-bottom: 1rem; border-bottom: 1px solid #e9ecef; align-items: flex-start; }}
            .activity-item:last-child {{ border-bottom: none; }}
            .activity-icon {{ font-size: 1.2rem; margin-top: 2px; }}
            .activity-details strong {{ font-weight: 600; }}
            .activity-details em {{ color: #495057; }}
            .activity-time {{ font-size: 0.8rem; color: #6c757d; }}
        </style>
    </head>
    <body>
        <div class="container">
            <h1>🦆 Duck Issue Tracker</h1>
            <div class="page-grid">
                <div class="main-content">
                    <p>Generated on: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}</p>
                    {branch_info_html}
                    <div class="tab-buttons">
                        <button class="tab-button active" onclick="openTab(event, 'open')">Open ({len(open_issues)})</button>
                        <button class="tab-button" onclick="openTab(event, 'closed')">Closed ({len(closed_issues)})</button>
                    </div>
                    <div id="open" class="tab-content active">
                        <table class="issue-table"><tbody>{open_issues_html}</tbody></table>
                    </div>
                    <div id="closed" class="tab-content">
                        <table class="issue-table"><tbody>{closed_issues_html}</tbody></table>
                    </div>
                </div>
                <aside class="activity-log-container">
                    <h2>Activity Log</h2>
                    {activity_log_html}
                </aside>
            </div>
            <div class="footer">Report generated by Duck Issue Tracker.</div>
        </div>
        <script>
            function toggleDetails(row) {{
                row.nextElementSibling.style.display = row.nextElementSibling.style.display === 'table-row' ? 'none' : 'table-row';
            }}
            function openTab(evt, tabName) {{
                let i, tabcontent, tablinks;
                tabcontent = document.getElementsByClassName("tab-content");
                for (i = 0; i < tabcontent.length; i++) {{ tabcontent[i].style.display = "none"; }}
                tablinks = document.getElementsByClassName("tab-button");
                for (i = 0; i < tablinks.length; i++) {{ tablinks[i].className = tablinks[i].className.replace(" active", ""); }}
                document.getElementById(tabName).style.display = "block";
                evt.currentTarget.className += " active";
            }}
        </script>
    </body>
    </html>
    """
    with open(report_path, 'w', encoding='utf-8') as f:
        f.write(html_content)

    print(f"Successfully generated HTML report at: {os.path.abspath(report_path)}")

def main():
    print("Starting issue scan for the Duck Issue Tracker...")
    try:
        import requests
    except ImportError:
        print("\nError: The 'requests' library is required. Please install it using 'pip install requests'")
        return

    git_info = get_git_info()
    if not git_info:
        print("Error: This script must be run inside a git repository for full functionality.")

    scan_directory = TARGET_DIRECTORY if TARGET_DIRECTORY else os.getcwd()
    print(f"Scanning directory: {os.path.abspath(scan_directory)}")

    source_files = get_all_source_files(scan_directory)
    print(f"Found {len(source_files)} source files to analyze.")

    all_found_issues = [issue for file_path in source_files for issue in find_issues_in_file(file_path, scan_directory)]
    print(f"Found {len(all_found_issues)} issues in total across all files.")

    updated_count, closed_count = update_issue_database(all_found_issues)
    print(f"Updated/created {updated_count} issue records.")
    print(f"Marked {closed_count} issues as closed.")

    generate_html_report(git_info)

    print("\nScan complete.")

if __name__ == "__main__":
    main()
