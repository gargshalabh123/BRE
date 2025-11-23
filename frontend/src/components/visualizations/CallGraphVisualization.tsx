import React, { useState, useEffect, useRef } from 'react';
import {
  Box,
  Card,
  CardContent,
  Typography,
  Alert,
  CircularProgress,
  FormControl,
  InputLabel,
  Select,
  MenuItem,
  Chip,
} from '@mui/material';
import api from '../../services/api';

interface CallGraphVisualizationProps {
  uploadId: string;
  programs: any[];
}

const CallGraphVisualization: React.FC<CallGraphVisualizationProps> = ({
  uploadId,
  programs,
}) => {
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [selectedProgram, setSelectedProgram] = useState<string>('');
  const [callData, setCallData] = useState<any>(null);
  const canvasRef = useRef<HTMLCanvasElement>(null);

  useEffect(() => {
    if (selectedProgram) {
      loadCallGraph();
    }
  }, [selectedProgram]);

  useEffect(() => {
    if (callData) {
      renderGraph();
    }
  }, [callData]);

  const loadCallGraph = async () => {
    setLoading(true);
    setError(null);

    try {
      // Load callers and callees
      const [callersData, calleesData] = await Promise.all([
        api.getProgramCallers(uploadId, selectedProgram),
        api.getProgramCallees(uploadId, selectedProgram),
      ]);

      setCallData({
        program: selectedProgram,
        callers: callersData.callers,
        callees: calleesData.callees,
      });

      setLoading(false);
    } catch (err: any) {
      setError(err.message || 'Failed to load call graph');
      setLoading(false);
    }
  };

  const renderGraph = () => {
    const canvas = canvasRef.current;
    if (!canvas || !callData) return;

    const ctx = canvas.getContext('2d');
    if (!ctx) return;

    // Set canvas size
    canvas.width = canvas.offsetWidth;
    canvas.height = 600;

    // Clear canvas
    ctx.clearRect(0, 0, canvas.width, canvas.height);

    // Calculate layout
    const centerX = canvas.width / 2;
    const centerY = canvas.height / 2;
    const radius = 150;

    // Draw center node (selected program)
    drawNode(ctx, centerX, centerY, callData.program, '#1976d2', true);

    // Draw callers (left side)
    const callers = callData.callers || [];
    const callerAngleStep = Math.PI / (callers.length + 1);
    callers.forEach((caller: any, idx: number) => {
      const angle = Math.PI + callerAngleStep * (idx + 1) - Math.PI / 2;
      const x = centerX + Math.cos(angle) * radius * 1.5;
      const y = centerY + Math.sin(angle) * radius * 1.5;

      // Draw edge
      drawEdge(ctx, x, y, centerX, centerY, '#4caf50');

      // Draw node
      drawNode(ctx, x, y, caller.caller, '#4caf50', false);

      // Draw call type
      const midX = (x + centerX) / 2;
      const midY = (y + centerY) / 2;
      ctx.fillStyle = '#666';
      ctx.font = '10px Arial';
      ctx.fillText(caller.call_type, midX, midY);
    });

    // Draw callees (right side)
    const callees = callData.callees || [];
    const calleeAngleStep = Math.PI / (callees.length + 1);
    callees.forEach((callee: any, idx: number) => {
      const angle = calleeAngleStep * (idx + 1) - Math.PI / 2;
      const x = centerX + Math.cos(angle) * radius * 1.5;
      const y = centerY + Math.sin(angle) * radius * 1.5;

      // Draw edge
      drawEdge(ctx, centerX, centerY, x, y, '#f44336');

      // Draw node
      const isExternal = callee.is_external === 1;
      drawNode(ctx, x, y, callee.callee, isExternal ? '#ff9800' : '#f44336', false);

      // Draw call type
      const midX = (centerX + x) / 2;
      const midY = (centerY + y) / 2;
      ctx.fillStyle = '#666';
      ctx.font = '10px Arial';
      ctx.fillText(callee.call_type, midX, midY);
    });
  };

  const drawNode = (
    ctx: CanvasRenderingContext2D,
    x: number,
    y: number,
    label: string,
    color: string,
    isCenter: boolean
  ) => {
    const nodeRadius = isCenter ? 40 : 30;

    // Draw circle
    ctx.beginPath();
    ctx.arc(x, y, nodeRadius, 0, 2 * Math.PI);
    ctx.fillStyle = color;
    ctx.fill();
    ctx.strokeStyle = '#fff';
    ctx.lineWidth = 2;
    ctx.stroke();

    // Draw label
    ctx.fillStyle = '#fff';
    ctx.font = isCenter ? 'bold 12px Arial' : '10px Arial';
    ctx.textAlign = 'center';
    ctx.textBaseline = 'middle';

    // Truncate long labels
    const maxLength = isCenter ? 10 : 8;
    const displayLabel = label.length > maxLength ? label.substring(0, maxLength) + '...' : label;
    ctx.fillText(displayLabel, x, y);
  };

  const drawEdge = (
    ctx: CanvasRenderingContext2D,
    x1: number,
    y1: number,
    x2: number,
    y2: number,
    color: string
  ) => {
    ctx.beginPath();
    ctx.moveTo(x1, y1);
    ctx.lineTo(x2, y2);
    ctx.strokeStyle = color;
    ctx.lineWidth = 2;
    ctx.stroke();

    // Draw arrow
    const angle = Math.atan2(y2 - y1, x2 - x1);
    const arrowSize = 10;
    ctx.beginPath();
    ctx.moveTo(x2, y2);
    ctx.lineTo(
      x2 - arrowSize * Math.cos(angle - Math.PI / 6),
      y2 - arrowSize * Math.sin(angle - Math.PI / 6)
    );
    ctx.lineTo(
      x2 - arrowSize * Math.cos(angle + Math.PI / 6),
      y2 - arrowSize * Math.sin(angle + Math.PI / 6)
    );
    ctx.closePath();
    ctx.fillStyle = color;
    ctx.fill();
  };

  return (
    <Box>
      <Card sx={{ mb: 3 }}>
        <CardContent>
          <FormControl fullWidth>
            <InputLabel>Select Program to Visualize</InputLabel>
            <Select
              value={selectedProgram}
              onChange={(e) => setSelectedProgram(e.target.value)}
              label="Select Program to Visualize"
            >
              {programs.map((prog: any) => (
                <MenuItem key={prog.program_name} value={prog.program_name}>
                  {prog.program_name}
                  {prog.entry_point === 1 && (
                    <Chip label="Entry Point" size="small" sx={{ ml: 1 }} />
                  )}
                </MenuItem>
              ))}
            </Select>
          </FormControl>
        </CardContent>
      </Card>

      {loading && (
        <Box display="flex" justifyContent="center" p={4}>
          <CircularProgress />
        </Box>
      )}

      {error && (
        <Alert severity="error" sx={{ mb: 2 }}>
          {error}
        </Alert>
      )}

      {callData && !loading && (
        <>
          <Card sx={{ mb: 2 }}>
            <CardContent>
              <Box display="flex" gap={2} flexWrap="wrap">
                <Box display="flex" alignItems="center" gap={1}>
                  <Box
                    sx={{
                      width: 20,
                      height: 20,
                      bgcolor: '#4caf50',
                      borderRadius: '50%',
                    }}
                  />
                  <Typography variant="body2">
                    Callers ({callData.callers.length})
                  </Typography>
                </Box>
                <Box display="flex" alignItems="center" gap={1}>
                  <Box
                    sx={{
                      width: 20,
                      height: 20,
                      bgcolor: '#1976d2',
                      borderRadius: '50%',
                    }}
                  />
                  <Typography variant="body2">Selected Program</Typography>
                </Box>
                <Box display="flex" alignItems="center" gap={1}>
                  <Box
                    sx={{
                      width: 20,
                      height: 20,
                      bgcolor: '#f44336',
                      borderRadius: '50%',
                    }}
                  />
                  <Typography variant="body2">
                    Callees ({callData.callees.length})
                  </Typography>
                </Box>
                <Box display="flex" alignItems="center" gap={1}>
                  <Box
                    sx={{
                      width: 20,
                      height: 20,
                      bgcolor: '#ff9800',
                      borderRadius: '50%',
                    }}
                  />
                  <Typography variant="body2">External Programs</Typography>
                </Box>
              </Box>
            </CardContent>
          </Card>

          <Card>
            <CardContent>
              <canvas
                ref={canvasRef}
                style={{ width: '100%', height: '600px', border: '1px solid #ddd' }}
              />
            </CardContent>
          </Card>
        </>
      )}

      {!selectedProgram && !loading && (
        <Alert severity="info">Select a program to visualize its call graph</Alert>
      )}
    </Box>
  );
};

export default CallGraphVisualization;
