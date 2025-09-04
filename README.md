# Friendbook 📖✨

A modern digital friendship book application built with Pascal/Delphi for collecting and managing friends' profiles with photos, personal information, and entertainment preferences.

## 🌟 Features

### 📸 Profile Management
- Complete personal profiles with photos
- Automatic image file management (BMP format)
- Safe filename generation with timestamp uniqueness
- Flexible 5-part address system

### 🎬 Entertainment Tracking
- Favorite movies collection with posters and descriptions
- TV series preferences with detailed information
- TMDB-style data structure support*

*Require Internet Connection

### 💾 Data Storage
- Optimized JSON serialization (~40% size reduction)
- Compact field names for efficient storage
- Separate image file storage to keep JSON lightweight
- Intelligent text truncation to prevent oversized files

### 🏠 Personal Information
- Basic info (name, birthday, nicknames)
- Contact details with flexible address fields
- Professional information (job, volunteer work)
- Personal details (hobbies, religious affiliation, marital status)
- Fun facts and additional descriptions

## 🚀 Quick Start

### Prerequisites
- Delphi/Lazarus IDE with Pascal compiler
- Windows environment (uses VCL components)
- Basic understanding of Object Pascal

### Installation
1. Clone the repository:
   ```bash
   git clone https://github.com/DevTimmOfTO/Friendbook.git
   ```
2. Open the project in your Delphi/Lazarus IDE
3. Compile and run the application

### Basic Usage

#### Creating a new person profile:
```pascal
var
  Person: TPerson;
  JSON: TJSONObject;
begin
  Person := TPerson.Create;
  try
    // Set basic information
    Person.FirstName := 'Max';
    Person.Surname := 'Mustermann';
    Person.Birthday := EncodeDate(1995, 8, 15);
    Person.Hobbies := 'Gaming, Programming, Music';
    
    // Add to global list
    PersonList.Add(Person);
    
    // Save to JSON
    JSON := Person.ToJSON('C:\MyFriendshipBook\');
    // ... save JSON to file
  finally
    JSON.Free;
  end;
end;
```

#### Loading from JSON:
```pascal
var
  Person: TPerson;
  LoadedJSON: TJSONObject;
begin
  // ... load JSON from file
  Person := TPerson.CreateFromJSON(LoadedJSON, 'C:\MyFriendshipBook\');
  PersonList.Add(Person);
  
  ShowMessage('Loaded: ' + Person.GetFullName + 
             ' (Age: ' + IntToStr(Person.GetAge) + ')');
end;
```

## 📁 File Structure

```
Freundschaftsbuch/
├── PersonData.pas          # Main data structures and logic
├── README.md              # This file
├── LICENSE                # MIT License
└── ProfileImages/         # Auto-created folder for profile pictures
    ├── Max_Mustermann_2025-01-15_14-30-45.bmp
    └── ...
```

## 🏗️ Architecture

### Core Components

#### `TMovieSeriesEntry` Record
- Stores movie/TV series information
- JSON serialization with overview truncation (100 chars)
- Designed for entertainment preference tracking

#### `TPerson` Class
- Complete person profile management
- Automatic image file handling
- Optimized JSON serialization/deserialization
- Memory-efficient data structures

#### Global Management
- `PersonList: TObjectList<TPerson>` for centralized person management
- Automatic memory management
- Thread-safe operations

### Data Storage Strategy

#### JSON Optimization
- **Shortened field names**: `fn` instead of `FirstName` (saves ~40% space)
- **Intelligent truncation**: Long descriptions automatically shortened
- **Selective storage**: Only non-empty fields are saved
- **Separate image files**: Profile pictures stored as external BMP files

#### Field Name Mappings
| Full Name | JSON Key | Description |
|-----------|----------|-------------|
| FirstName | `fn` | Person's first name |
| Surname | `sn` | Person's surname |
| Birthday | `bd` | ISO8601 formatted date |
| Description | `desc` | Bio/description (max 500 chars) |
| Nicknames | `nn` | Array of nicknames (max 3) |
| Address | `addr` | Compact address string |
| Profession | `prof` | Job title |
| Hobbies | `hob` | Hobbies and interests |
| FavoriteMovies | `mov` | Array of movie entries |
| FavoriteSeries | `ser` | Array of series entries |

## 🖼️ Image Management

### Storage Locations
- **Default**: `%DOCUMENTS%/FreundschaftsbuchApp/ProfileImages/`
- **Custom**: Next to JSON file in `ProfileImages/` subfolder

### File Naming Convention
```
PersonName_YYYY-MM-DD_HH-NN-SS.bmp
```
Example: `Max_Mustermann_2025-01-15_14-30-45.bmp`

### Safety Features
- Invalid filesystem characters replaced with underscores
- Timestamp ensures uniqueness
- Automatic cleanup of old image files
- Graceful handling of missing/corrupted images

## 🎯 Use Cases

### Digital Friendship Books
- School class memories
- Summer camp participants
- Club member directories
- Family reunion attendees

### Social Applications
- Contact management with rich profiles
- Entertainment preference matching
- Social network profile backup
- Community member directories

### Data Collection
- Survey participants with photos
- Event attendee information
- Customer profile management
- Volunteer database maintenance

## ⚡ Performance Features

### Memory Efficiency
- Lazy loading of profile images
- Automatic cleanup of unused image files
- Optimized list management with `TObjectList`
- Smart memory allocation for large datasets

### Storage Optimization
- Compressed JSON with shortened field names
- Intelligent text truncation
- Separate binary file storage
- Minimal redundancy in data structure

### Error Handling
- Graceful handling of missing image files
- Safe JSON parsing with null checks
- Automatic recovery from corrupted data
- Robust filesystem operations

## 🛠️ Development

### Code Structure
```pascal
// Main data management
unit PersonData;

// Core classes
TMovieSeriesEntry = record  // Entertainment data
TPerson = class            // Complete person profile

// Global management
var PersonList: TObjectList<TPerson>;
```

### Key Methods
- `ToJSON(BasePath)`: Serialize to optimized JSON
- `FromJSON(JSON, BasePath)`: Deserialize from JSON
- `GetFullName()`: Formatted name display
- `GetAge()`: Calculate current age
- Image management: `SaveProfileImageToFile()`, `LoadProfileImageFromFile()`

### Extension Points
- Additional image formats (PNG, JPG support)
- Database backend integration
- Web API compatibility
- Advanced search functionality
- Data export formats (CSV, XML)

## 📊 Technical Specifications

### Supported Data Types
- **Text**: UTF-8 strings with intelligent truncation
- **Dates**: Full DateTime support with ISO8601 serialization
- **Images**: BMP format with automatic conversion
- **Arrays**: Dynamic lists for nicknames, movies, series
- **Addresses**: Flexible 5-field system for international compatibility

### Performance Metrics
- **JSON size reduction**: ~40% compared to full field names
- **Image loading**: Lazy loading prevents memory bloat
- **Startup time**: Instant with deferred image loading
- **Memory usage**: Optimized for large contact lists (1000+ entries)

### Compatibility
- **OS**: Windows (VCL-dependent)
- **Compiler**: Delphi 10+ or Lazarus/FPC
- **JSON**: Standard RFC 7159 compliant
- **Images**: Windows BMP format for maximum compatibility

## 🔒 Data Privacy

### Local Storage
- All data stored locally on user's machine
- No external API calls required
- Complete user control over data location
- No cloud dependencies

### Security Features
- Safe filename generation prevents directory traversal
- Input validation on all user data
- Graceful handling of malformed JSON
- No external network access required

## 🤝 Contributing

### Development Setup
1. Fork the repository
2. Create a feature branch: `git checkout -b feature-name`
3. Make changes with proper documentation
4. Test with various data scenarios
5. Submit a pull request

### Code Style Guidelines
- Comprehensive documentation for all public methods
- Error handling for all file operations
- Memory leak prevention with proper cleanup
- Consistent naming conventions

### Feature Requests
- Image format expansion (PNG, JPG)
- Database backend options
- Import/export functionality
- Advanced search capabilities
- Mobile compatibility layer

## 📄 License

MIT License

Copyright (c) 2025 Timm Johannes Göring

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

## 📞 Contact

- **Author**: Timm Johannes Göring
- **Project**: Friendship Book 
- **Language**: Object Pascal/Delphi
- **Version**: 1.2

---

*Built with ❤️ for preserving memories and friendships in the digital age.*
