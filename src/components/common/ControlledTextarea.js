import React, { useRef, useEffect } from 'react';
import styled from 'styled-components';

const ControlledTextarea = ({ 
  value, 
  onChange,
  placeholder, 
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
    <StyledTextareaContainer>
      <StyledTextarea
        ref={textareaRef}
        value={value}
        onChange={handleChange}
        className={value ? 'has-value' : ''}
        {...props}
      />
      {placeholder && !value && (
        <PlaceholderText>{placeholder}</PlaceholderText>
      )}
    </StyledTextareaContainer>
  );
};

const StyledTextareaContainer = styled.div`
  position: relative;
  width: 100%;
`;

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

export default ControlledTextarea;