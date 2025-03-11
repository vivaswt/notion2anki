# notion2anki

`notion2anki` is a Haskell application that integrates Notion with Anki, allowing you to create Anki flashcards from Notion database entries.

## Features

- Fetch flashcards from a Notion database
- Add flashcards to Anki with support for images and audio
- Interactive confirmation before adding flashcards

## Installation

1. Clone the repository:
    ```sh
    git clone https://github.com/githubuser/notion2anki.git
    cd notion2anki
    ```

2. Install dependencies using Stack:
    ```sh
    stack setup
    stack build
    ```

## Usage

1. Set the `PRIVATE_NOTION_TOKEN` environment variable with your Notion integration token:
    
```sh    export PRIVATE_NOTION_TOKEN="your-notion-token"
    ```

2. Run the application:
    ```sh
    stack exec notion2anki
    ```

## Configuration

The application uses a Notion database with specific properties for flashcards. Ensure your Notion database has the following properties:
- `問題` (Question)
- `答え` (Answer)
- `発音` (IPA)
- `意味` (Sense)
- `その他発音` (Other IPA)
- `語呂` (Mnemonic)

## Contributing

Contributions are welcome! Please open an issue or submit a pull request on GitHub.

## License

This project is licensed under the BSD-3-Clause License. See the  file for details.