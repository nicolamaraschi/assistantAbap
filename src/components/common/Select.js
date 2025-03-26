import React from 'react';
import styled from 'styled-components';

// Componente Select riutilizzabile
const Select = ({
  id,
  name,
  value,
  onChange,
  options = [],
  placeholder = 'Seleziona un\'opzione',
  disabled = false,
  required = false,
  className,
  ...rest
}) => {
  return (
    <StyledSelect
      id={id}
      name={name}
      value={value}
      onChange={onChange}
      disabled={disabled}
      required={required}
      className={className}
      {...rest}
    >
      {placeholder && (
        <option value="" disabled>
          {placeholder}
        </option>
      )}
      
      {options.map((option, index) => {
        // Gestisce array di oggetti con label/value o array di optgroup
        if (option.options) {
          return (
            <optgroup key={index} label={option.label}>
              {option.options.map((subOption, subIndex) => (
                <option 
                  key={`${index}-${subIndex}`}
                  value={subOption.value || subOption.id} 
                  disabled={subOption.disabled}
                >
                  {subOption.label || subOption.name}
                </option>
              ))}
            </optgroup>
          );
        }
        
        return (
          <option 
            key={index} 
            value={option.value || option.id} 
            disabled={option.disabled}
          >
            {option.label || option.name}
          </option>
        );
      })}
    </StyledSelect>
  );
};

// Stili con styled-components
const StyledSelect = styled.select`
  display: block;
  width: 100%;
  padding: 10px;
  border: 1px solid #ddd;
  border-radius: 4px;
  font-size: 15px;
  transition: border-color 0.3s, box-shadow 0.3s;
  appearance: none; /* Rimuove lo stile di default del browser */
  background-image: url("data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='24' height='24' viewBox='0 0 24 24' fill='none' stroke='%23333' stroke-width='2' stroke-linecap='round' stroke-linejoin='round'%3E%3Cpolyline points='6 9 12 15 18 9'%3E%3C/polyline%3E%3C/svg%3E");
  background-repeat: no-repeat;
  background-position: right 10px center;
  background-size: 16px;
  background-color: white;
  text-overflow: ellipsis;
  
  &:focus {
    outline: none;
    border-color: #0066cc;
    box-shadow: 0 0 0 3px rgba(0, 102, 204, 0.2);
  }
  
  &:hover {
    border-color: #bbb;
  }
  
  &:disabled {
    background-color: #f5f5f5;
    cursor: not-allowed;
    opacity: 0.7;
  }
  
  /* Stile per optgroup */
  optgroup {
    font-weight: bold;
  }
  
  option {
    padding: 10px;
    white-space: normal;
    min-width: 100%;
  }
`;

export default Select;
