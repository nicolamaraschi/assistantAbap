import React, { useRef, useEffect } from 'react';
import styled from 'styled-components';

const ControlledTextarea = ({ 
  value, 
  onChange, 
  ...props 
}) => {
  const textareaRef = useRef(null);
  const cursorPositionRef = useRef(null);
  
  const handleChange = (e) => {
    cursorPositionRef.current = e.target.selectionStart;
    onChange(e);
  };
  
  useEffect(() => {
    if (textareaRef.current && cursorPositionRef.current !== null) {
      const textarea = textareaRef.current;
      const position = cursorPositionRef.current;
      
      requestAnimationFrame(() => {
        if (document.activeElement === textarea) {
          textarea.selectionStart = position;
          textarea.selectionEnd = position;
        }
      });
    }
  }, [value]);
  
  return (
    <StyledTextarea
      ref={textareaRef}
      value={value}
      onChange={handleChange}
      {...props}
    />
  );
};

const StyledTextarea = styled.textarea`
  width: 100%;
  padding: 10px;
  border: 1px solid #ddd;
  border-radius: 4px;
  font-size: 15px;
  transition: border-color 0.3s, box-shadow 0.3s;
  font-family: 'Courier New', monospace;
  resize: vertical;
  min-height: 80px;
  
  &:focus {
    outline: none;
    border-color: #0066cc;
    box-shadow: 0 0 0 3px rgba(0, 102, 204, 0.2);
  }
`;

export default ControlledTextarea;
