
import subprocess

def Settings( **kwargs ):
    
    completed = subprocess.run(["pipenv", "run", "which", "python"],
        capture_output=True)
    python_path = completed.stdout.decode().strip()

    return {
      'interpreter_path': python_path
    }
