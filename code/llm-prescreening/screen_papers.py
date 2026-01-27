import openai 
from openai import OpenAI
import pandas as pd
from tqdm import tqdm
import re
import json

def extract_json(answer):
    # Remove markdown formatting (e.g., ```json ... ```)
    cleaned = re.sub(r"```(json)?", "", answer, flags=re.IGNORECASE).strip("` \n")
    try:
        return json.loads(cleaned)
    except json.JSONDecodeError:
        return None

openai.api_key = apikey  
openai.api_base = "https://openrouter.ai/api/v1"

client = OpenAI(
    api_key = openai.api_key,
    base_url = openai.api_base
)

def screen_papers(df, inclusion_criteria, exclusion_criteria, mod):
    decisions = []
    reasons = []
    
    total_input_tokens = 0
    total_output_tokens = 0

    prompt_template = (
        f"You are helping with a systematic review. Classify the following paper based on the inclusion and exclusion criteria.\n\n"
        f"Inclusion: {inclusion_criteria}\n\n"
        f"Exclusion: {exclusion_criteria}\n\n"
        "Paper:\nTitle: {title}\nAbstract: {abstract}\n\n""Answer format (use JSON):\n"
        "{{\n  \"decision\": \"include / exclude / unsure\",\n  \"reason\": \"<short reason>\"\n}}\n"
        "Only use the terms 'include', 'exclude', or 'unsure' as the decision."
    )

    for _, row in tqdm(df.iterrows(), total=len(df)):
        input_text = prompt_template.format(title=row["title"], abstract=row["abstract"])
        
        decision, reason = "unsure", "No response"

        for attempt in range(3):
            try:
                response = client.chat.completions.create(
                    model=mod,
                    messages=[{"role": "user", "content": input_text}],
                    temperature=0.2,
                    max_tokens=150
                )
        
                usage = response.usage
                total_input_tokens += usage.prompt_tokens
                total_output_tokens += usage.completion_tokens
        
                answer = response.choices[0].message.content.strip()
                parsed = extract_json(answer)
        
                if parsed:
                    decision = parsed.get("decision", "unsure").lower()
                    reason = parsed.get("reason", "").strip()
                    if decision not in ["include", "exclude", "unsure"]:
                        decision = "unsure"
                else:
                    decision = "unsure"
                    reason = "Failed to parse JSON: " + answer[:300]
                    print("⚠️ Could not parse JSON. Raw answer:\n", answer)
        
                break  # success → exit retry loop
        
            except openai.RateLimitError:
                time.sleep(5 * (attempt + 1))
            except Exception as e:
                decision = "unsure"
                reason = f"Error: {str(e)}"
                break

        decisions.append(decision)
        reasons.append(reason[:500])

    df["llm_decision"] = decisions
    df["llm_reason"] = reasons

    print(f"Total input tokens: {total_input_tokens}")
    print(f"Total output tokens: {total_output_tokens}")
    print(f"Estimated cost: ${(total_input_tokens/1000)*0.01 + (total_output_tokens/1000)*0.03:.2f}")

    return df
