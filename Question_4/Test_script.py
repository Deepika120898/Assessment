import json
import os
import pandas as pd

from question_4_genai_clinical_data_assistant import ClinicalTrialDataAgent

ae = pd.read_csv("question_4_genai/adae.csv")

# Default to OpenAI if key exists; otherwise run mock (so reviewer can run it)
mode = "openai" if os.getenv("OPENAI_API_KEY") else "mock"

agent = ClinicalTrialDataAgent(ae_df=ae, mode=mode)

test_questions = [
    "Give me the subjects who had Adverse events of Moderate severity.",
    "Show me the subjects who had Headache.",
    "Which subjects had adverse events in the Skin body system?"
]

for i, q in enumerate(test_questions, start=1):
    resp = agent.ask(q)
    print(f"\n--- Query {i} ---")
    print("Question:", resp["question"])
    print("Parsed Output:", json.dumps(resp["parsed_output"], indent=2))
    print("Unique Subject Count:", resp["result"]["unique_subject_count"])
    print("Subject IDs:", resp["result"]["subject_ids"])
    
