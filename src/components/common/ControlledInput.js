import React, { useRef, useLayoutEffect } from 'react';
import styled from 'styled-components';

const ControlledInput = ({ 
  type = 'text', 
  value, 
  onChange, 
  placeholder,
  ...props 
}) => {
  const inputRef = useRef(null);
  const cursorPositionRef = useRef(null);
  
  const handleChange = (e) => {
    // Salva la posizione del cursore
    cursorPositionRef.current = e.target.selectionStart;
    onChange(e);
  };
  
  // useLayoutEffect è sincrono e viene eseguito prima del painting del browser
  useLayoutEffect(() => {
    const input = inputRef.current;
    if (input && cursorPositionRef.current !== null && document.activeElement === input) {
      input.selectionStart = cursorPositionRef.current;
      input.selectionEnd = cursorPositionRef.current;
    }
  });
  
  return (
    <StyledInputContainer>
      <StyledInput
        ref={inputRef}
        type={type}
        value={value}
        onChange={handleChange}
        className={value ? 'has-value' : ''}
        {...props}
      />
      {placeholder && !value && (
        <PlaceholderText>{placeholder}</PlaceholderText>
      )}
    </StyledInputContainer>
  );
};

const StyledInputContainer = styled.div`
  position: relative;
  width: 100%;
`;

const StyledInput = styled.input`
  width: 100%;
  padding: 10px;
  border: 1px solid #ddd;
  border-radius: 4px;
  font-size: 15px;
  transition: border-color 0.3s, box-shadow 0.3s;
  font-family: 'Courier New', monospace;
  background-color: transparent;
  z-index: 2;
  position: relative;
  
  &:focus {
    outline: none;
    border-color: #0066cc;
    box-shadow: 0 0 0 3px rgba(0, 102, 204, 0.2);
  }
  
  &.has-value + .placeholder {
    display: none;
  }
`;

const PlaceholderText = styled.div`
  position: absolute;
  top: 10px;
  left: 10px;
  color: #aaa;
  pointer-events: none;
  z-index: 1;
  font-family: 'Courier New', monospace;
  font-size: 15px;
`;

export default ControlledInput;