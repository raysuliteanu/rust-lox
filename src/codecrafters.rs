// this is for codecrafters specifically
pub fn format_float(number: f64) -> String {
    // Use high precision initially
    let mut formatted = format!("{:.10}", number);
    // Remove trailing zeros
    formatted = formatted.trim_end_matches('0').to_string();
    // ensure all formatted numbers end in ".0"
    if formatted.ends_with('.') {
        formatted.push('0');
    }

    formatted
}

