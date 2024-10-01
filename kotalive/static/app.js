document.addEventListener('DOMContentLoaded', function() {
    const inputBox = document.getElementById('inputBox');
    const tokenizeButton = document.getElementById('tokenizeButton');
    const tokensDisplay = document.getElementById('tokensDisplay');

    tokenizeButton.addEventListener('click', async () => {
        const inputText = inputBox.value;
        const response = await fetch('/tokenize', {
            method: 'POST',
            headers: {
                'Content-Type': 'text/plain'
            },
            body: inputText
        });

        if (response.ok) {
            const tokensResponse = await response.json();
            tokensDisplay.innerHTML = tokensResponse.tokens.join(', ');
        } else {
            console.error('Failed to fetch tokens:', response.statusText);
        }
    });
});

