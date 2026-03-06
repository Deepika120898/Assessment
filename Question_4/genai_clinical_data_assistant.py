
import os
import re
import json
from typing import Dict, Any, List

import pandas as pd


class ClinicalTrialDataAgent:
    """
    ClinicalTrialDataAgent:
    - Takes a user's natural language question
    - Uses LLM (OpenAI) OR mock parser to convert it to a structured JSON:
        { "target_column": "...", "filter_value": "..." }
    - Executes the filter on the AE dataframe
    - Returns unique subject count + subject IDs
    """

    def __init__(self, ae_df: pd.DataFrame):
        self.df = ae_df.copy()

        required_cols = {"USUBJID", "AESEV", "AETERM", "AESOC"}
        missing = required_cols - set(self.df.columns)
        if missing:
            raise ValueError(f"Missing required columns in adae.csv: {sorted(missing)}")

        # Requirement #1: Schema definition
        self.schema = {
            "USUBJID": "Unique subject identifier",
            "AESEV": "Adverse event severity/intensity (MILD, MODERATE, SEVERE)",
            "AETERM": "Adverse event term (e.g., Headache, Nausea)",
            "AESOC": "System organ class / body system (e.g., Cardiac disorders)"
        }

    def build_prompt(self, question: str) -> str:
        # Requirement #2: Prompt
        schema_text = "\n".join([f"- {k}: {v}" for k, v in self.schema.items()])

        return f"""
You are a clinical data assistant.
You must map the user's question to ONE dataset column and extract the filter value.

Dataset schema:
{schema_text}

Mapping rules:
- severity/intensity -> AESEV
- condition/AE term -> AETERM
- body system/SOC -> AESOC

Return ONLY valid JSON with this exact format:
{{"target_column":"AESEV|AETERM|AESOC", "filter_value":"<value>"}}

User question:
{question}
""".strip()

    def parse_with_mock(self, question: str) -> Dict[str, str]:
        """
        Mock LLM parser. Works without API key.
        """
        q = question.lower()

        # Severity mapping
        sev = re.search(r"\b(mild|moderate|severe)\b", q)
        if "severity" in q or "intensity" in q or sev:
            if sev:
                return {"target_column": "AESEV", "filter_value": sev.group(1).upper()}

        # Body system mapping
        if "soc" in q or "body system" in q or "system organ class" in q:
            # Simple value extraction: try to grab words after "in" or "of"
            m = re.search(r"\b(in|of)\s+([a-z\s]+)$", q)
            if m:
                return {"target_column": "AESOC", "filter_value": m.group(2).strip().title()}
            return {"target_column": "AESOC", "filter_value": "Cardiac disorders"}

        # Condition mapping (AETERM) - try to extract after "had" or "with"
        m = re.search(r"\b(had|with)\s+([a-z0-9\s\-\/]+)", q)
        if m:
            return {"target_column": "AETERM", "filter_value": m.group(2).strip().title()}

        # Default fallback
        return {"target_column": "AETERM", "filter_value": question.strip().title()}

    def parse_with_openai(self, question: str) -> Dict[str, str]:
        """
        Uses OpenAI only if OPENAI_API_KEY is set.
        If it fails, falls back to mock.
        """
        api_key = os.getenv("OPENAI_API_KEY")
        if not api_key:
            return self.parse_with_mock(question)

        try:
            from openai import OpenAI
            client = OpenAI(api_key=api_key)

            prompt = self.build_prompt(question)

            resp = client.chat.completions.create(
                model="gpt-4.1-mini",
                messages=[
                    {"role": "system", "content": "Return ONLY JSON. No extra text."},
                    {"role": "user", "content": prompt},
                ],
                temperature=0,
            )

            text = resp.choices[0].message.content.strip()
            data = json.loads(text)

            # Validate required keys
            if "target_column" not in data or "filter_value" not in data:
                return self.parse_with_mock(question)

            # Validate allowed columns
            if data["target_column"] not in ["AESEV", "AETERM", "AESOC"]:
                return self.parse_with_mock(question)

            return {"target_column": data["target_column"], "filter_value": str(data["filter_value"])}

        except Exception:
            # Any error -> fallback to mock so script still runs
            return self.parse_with_mock(question)

    def execute(self, parsed: Dict[str, str]) -> Dict[str, Any]:
        """
        Requirement #3: Apply filter and return unique subject count + IDs
        """
        col = parsed["target_column"]
        val = parsed["filter_value"]

        if col == "AESEV":
            mask = self.df[col].astype(str).str.upper().eq(val.upper())
        else:
            mask = self.df[col].astype(str).str.contains(re.escape(val), case=False, na=False)

        ids = (
            self.df.loc[mask, "USUBJID"]
            .dropna()
            .astype(str)
            .drop_duplicates()
            .tolist()
        )

        return {"unique_subject_count": len(ids), "subject_ids": ids}

    def ask(self, question: str) -> Dict[str, Any]:
        parsed = self.parse_with_openai(question)
        result = self.execute(parsed)
        return {"question": question, "parsed": parsed, "result": result}
