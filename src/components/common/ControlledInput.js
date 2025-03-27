import React, { useRef, useEffect } from 'react';
import styled from 'styled-components';

const ControlledInput = ({ 
  type = 'text', 
  value, 
  onChange, 
  ...props 
}) => {
  const inputRef = useRef(null);
  const cursorPositionRef = useRef(null);
  
  const handleChange = (e) => {
    cursorPositionRef.current = e.target.selectionStart;
    onChange(e);
  };
  
  useEffect(() => {
    if (inputRef.current && cursorPositionRef.current !== null) {
      const input = inputRef.current;
      const position = cursorPositionRef.current;
      
      requestAnimationFrame(() => {
        if (document.activeElement === input) {
          input.selectionStart = position;
          input.selectionEnd = position;
        }
      });
    }
  }, [value]);
  
  return (
    <StyledInput
      ref={inputRef}
      type={type}
      value={value}
      onChange={handleChange}
      {...props}
    />
  );
};

const StyledInput = styled.input`
  width: 100%;
  padding: 10px;
  border: 1px solid #ddd;
  border-radius: 4px;
  font-size: 15px;
  transition: border-color 0.3s, box-shadow 0.3s;
  font-family: 'Courier New', monospace;
  
  &:focus {
    outline: none;
    border-color: #0066cc;
    box-shadow: 0 0 0 3px rgba(0, 102, 204, 0.2);
  }
`;

export default ControlledInput;
