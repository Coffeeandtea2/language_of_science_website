import os

folders = [
    "app", "app/templates", "static", "quartopages"
]

files = [
    "app/__init__.py", "app/routes.py", "app/models.py",
    "app/templates/login.html", "app/templates/register.html",
    "app/templates/dashboard.html", "app/templates/progress.html",
    "run.py", "requirements.txt"
]

for folder in folders:
    os.makedirs(folder, exist_ok=True)

for file in files:
    with open(file, 'w', encoding='utf-8') as f:
        pass  # 빈 파일 생성
