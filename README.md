## Vesperin's Kiwi

The Kiwi API was implemented in Scala. It consists of two logical services: Curation and Parsing.

The Curation service manages Vesperin’s curation operations, such as basic code transformations and refactorings, code presentation, and publication (published on Twitter). The Parsing service manages the access to a Java parsing API, built on top of Eclipse JDT1.

All requests are, by default, provided as JSON. There is no authentication required to make Curation and Parsing API calls.

```
{"rename": {
  "what": "method",
  "where": [1, 6],
  "source": {
    "name": "...",
    "description": "...",
    "content": "..."
  }
 }
}
```

## License

    Copyright 2016 Huascar Sanchez

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.
