import gradio as gr

def greet(name, symbol, intensity):
    return f"{symbol * intensity} {name}! {symbol * intensity} "

demox = gr.Interface(
    fn=greet,
    inputs=["text", "text", "slider"],
    outputs=["text"],
)

demox.launch()
