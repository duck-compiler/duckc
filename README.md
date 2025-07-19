# Duck Lang - The Language for Hyperscalers

## TODO
## CLI

### dargo init

Erstellt eine Standard dargo.toml im aktuellen Directory.
Der Projekt Name ist der Name des aktuellen Directory.

> $ duck init

#### Optionale Flags:
| Name | Beschreibung |
-------- | --------
| `--lib` | Erzeugt das Projekt, sodass es direkt als Library konfiguriert ist. |

### duck new

Erstellt ein Directory mit dem gegebenen Namen.
Directory darf nicht existieren.
Wenn name nicht angegeben, dann wird nutzer nach namen geprompted.

> $ duck new <name>

#### Optionale Flags:
| Name | Beschreibung |
-------- | --------
| `--lib` | Erzeugt das Projekt, sodass es direkt als Library konfiguriert ist. |

### dargo build

Parsed die dargo.toml im aktuellen Directory.
Installiert alle benoetigten Dependencies.
Compiled das Projekt.
Wenn keine Binary angegeben, dann werden alle kompiliert.
Ansonsten nur die angegebene

> $ duck build [\<bin\>]

#### Optionale Flags:
| Name | Beschreibung | Default |
-------- | -------- | -------
|

### dargo run

Parsed die dargo.toml im aktuellen Directory.
Installiert alle benoetigten Dependencies.
Compiled das Projekt.

> $ duck run <bin>

#### Optionale Flags:
| Name | Beschreibung | Default |
-------- | -------- | -------
|

### duck compile
Kompiliert die gegebene Duck File

> $ duck compile <file.duck>

#### Optionale Flags:
| Name | Beschreibung | Default
-------- | -------- | ------
|--output <file_name>, -o <file_name>| Name fuer die ausgegebene binary | duck_out|
